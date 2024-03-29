{-# LANGUAGE KindSignatures, Rank2Types, TupleSections, PatternSynonyms #-}
module Gen.Reduce.Reduce where

import Conjure.Language.Domain
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.RunResult
import Gen.IO.ToolchainData
import Gen.Reduce.Data
import Gen.Reduce.Transform   (deEnum)
import Gen.Reduce.Random
import Gen.Instance.Point
import Gen.Reduce.Reduction
import Gen.Reduce.Runner
import Gen.Reduce.UnusedDomains
import System.FilePath          (takeExtension)
import Conjure.Language.Name
import Gen.Helpers.MonadNote
import Gen.Reduce.Point

import qualified Data.Map as M

reduceMain :: (MonadIO m, MonadLog m, RndGen m, MonadNote m)
           => Bool -> RState -> m RState
reduceMain check rr = do
  let base = (specDir_ . rconfig) rr
      fp   =  base </> "spec.spec.json"


  sp_ <- liftIO $ readFromJSON fp

  -- Remove quantification and enums
  let paramFp_ = base </> "given.param"
  paramFp <- liftIO $ doesFileExist paramFp_  >>= \case
    False -> return Nothing
    True  -> return (Just paramFp_)

  (sp,startParam') <-  liftIO $ deEnum sp_ paramFp
  liftIO $ checkForParamIfNeeded sp startParam'
  -- noteFormat "Starting with" [pretty sp, pretty startParam]
  let startParam = case startParam' of
        Just (Point []) -> Nothing
        x               -> x


  (errOccurs,_) <- case check of
                 False -> return (True, rr)
                 True -> (flip runReduceSettings) rr (return sp
                           >>= noteMsg "Checking if error still occurs"
                           >>=  (flip checkForError) (startParam)
                           >>= \case
                                  (Just ErrData{..}, _) -> do
                                    liftIO $ removeDirectoryRecursive (specDir)
                                    return True

                                  _ -> return False
                       )

  case errOccurs of
    False -> do
        note $  "Spec has no error with the given settings, not reducing"
        return rr
    True -> do
      (sfin,state) <-  (flip runReduceSettings) rr $
          return (sp, startParam)
          >>= doReductions
          >>= \ret -> get >>= \g -> addLog "FinalState" [pretty g] >> return ret


      noteFormat "FinalState" [pretty state]
      noteFormat "Start"  $ [pretty sp, maybe "" (("param" <+>) . pretty) startParam]

      end :: Maybe (Spec, Maybe Point)  <- case sfin of
               (Continue x)   -> return $  Just x
               NoTimeLeft{}   -> do
                  case mostReduced_ state of
                    Nothing -> return Nothing
                    Just (ErrData{..})  -> do
                       nsp <- liftIO $ readFromJSON  (specDir </> "spec.spec.json")
                       nmp <- liftIO $ readPointMay  (specDir </> "given.param")
                       return $ Just (nsp, nmp)

      let end2 :: Doc = case end of
            Nothing -> "Nothing"
            Just (nsp,nmp) -> if hashDoc nsp == hashDoc sp
                              && hashDoc startParam == hashDoc nmp then
                         "Nothing"
                       else
                          pretty nsp $$ maybe "" (("param" <+>) . pretty) nmp

      noteFormat "Final" $ [end2]

      return (state)

noteMsg :: MonadNote m => Doc -> b -> m b
noteMsg tx s = do
    noteFormat ("@" <+> tx) []
    return s


-- | The reduction process
doReductions :: (Spec, Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
doReductions start =
    return (Continue start)
    >>= con "tryRemoveConstraints" tryRemoveConstraints
    >>= con "loopToFixed"          (loopToFixed False)
    >>= con "eprimeAsSpec"         eprimeAsSpec


loopToFixed :: Bool -> (Spec,  Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
loopToFixed fin start = do
  res <-  return (Continue start)
      >>= con "removeObjective"      removeObjective
      >>= con "removeUnusedDomains"  removeUnusedDomains
      >>= con "removeConstraints"    removeConstraints
      >>= con "inlineGivens"         inlineGivens
      >>= con "simplyFinds"          simplyFinds
      >>= con "givensToFinds"        givensToFinds
      >>= con "simplyGivens"         simplyGivens
      >>= con "simplyConstraints"    simplyConstraints

  case res of
    (NoTimeLeft end) -> return $ NoTimeLeft end
    (Continue cur)   -> do
      de <- doExpensiveReductions
      -- We allow doing a reduction twice before giving up
      if hash start == hash cur then
        case (fin, de) of
          (False,_)    -> loopToFixed True cur
          (True,True)  -> return $ Continue start
          (True,False) -> do
            modify $ \st -> st{expensiveReductions_=True}
            note "! Enabling ExpensiveReductions"
            loopToFixed True cur

      else
        loopToFixed fin cur


tryRemoveConstraints :: (Spec,  Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
tryRemoveConstraints d@(Spec _ [] _ ,_)  = return . Continue $ d
tryRemoveConstraints d@(Spec ds _ obj, mp) = do
  timedSpec (Spec ds [] obj) mp f (fmap Continue . f)

  where
    f (Just r) = do
      recordResult r
      return $ (Spec ds [] obj, mp)

    f _ = return d


removeObjective :: (Spec,  Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
removeObjective d@(Spec _ _ Nothing,_)  = return . Continue $ d
removeObjective d@(Spec ds es Just{}, mp) =
  timedSpec (Spec ds es Nothing ) mp f (fmap Continue . f)

  where
    f (Just r) = do
      recordResult r
      return $ (Spec ds es Nothing, mp)

    f _ = return d

removeUnusedDomains :: (Spec,  Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
removeUnusedDomains d@(sp@(Spec ods es obj), mp) = do
    let unusedNames = unusedDomains sp

    res <- process (choices ods unusedNames)
    forM res $ \x -> case x of
      Nothing -> return d
      Just (ds,nmp) -> do
        return (Spec ds es obj, nmp)

    where
    choices :: Domains -> [Text] -> [Domains]
    choices ds ts =
        -- res would be [ [], [a], [b], [a,b],  ... ]
        let ways = orderedSubsequences ts
            res = fmap (\wy -> M.filterWithKey (\k _ -> k `notElem` wy) ds ) ways
        in res

    process :: [Domains]-> RRR (Timed (Maybe (Domains, Maybe Point)))
    process []     = return $ Continue $ Nothing
    process (x:xs) = timedSpec (Spec y es obj) p f g
        where
          y = ensureAFind x
          p = coordinateGivens y mp

          f (Just r) = do
            recordResult r
            return $ Just (y, p)

          f _  =  return $ Nothing

          g (Just r) = do
            recordResult r
            return . Continue . Just $ (y,p)

          g _ = process xs

inlineGivens :: (Spec,  Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
inlineGivens d@(_, Nothing) = return $ Continue $ d
inlineGivens d@((Spec ods es obj), Just (Point ops) ) = do
  res <- process givenSeq
  forM res $ \x -> case x of
    Nothing -> return d
    Just (fsp, fp) -> return (fsp,  emptyPointToNothing (Just fp) )

  where
  givenSeq = orderedSubsequences ops

  process :: [[(Name,Constant)]] -> RRR (Timed (Maybe (Spec, Point)))
  process []     = return $ Continue $ Nothing
  process (x:xs) = timedSpec theSpec (Just np) f g
    where

    theSpec = (Spec nds nes nobj)

    givens_rm = M.fromList $ x

    np   = Point [ (n,c) | (n,c) <- ops, n `M.notMember` givens_rm ]
    nds0  = M.filterWithKey (\k _ ->  (Name k) `M.notMember` givens_rm  ) ods

    nds  = M.map (second (transformGF) ) nds0
    nes  = map (transform inline) es
    nobj = fmap (second (transform inline) ) obj

    transformGF (Givenn v) = Givenn $ unEDom $ transform inline (EDom v)
    transformGF (Findd v)  = Findd  $ unEDom $ transform inline (EDom v)

    inline :: Expr -> Expr
    inline expr@(EVar (Var a _)) = case (Name a) `M.lookup` givens_rm of
                                     Nothing  -> expr
                                     Just c -> ECon c
    inline expr = expr


    f (Just r) = do
      recordResult r
      return $ Just (theSpec, np)

    f Nothing  =  return Nothing

    g (Just r) = do
      recordResult r
      return . Continue . Just $ (theSpec,np)

    g Nothing = process xs


removeConstraints :: (Spec,  Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
removeConstraints (Spec ds oes obj,mp) = do
    let nubbed = nub2 oes
    process (choices nubbed) >>= return . fmap (\x -> case x of
        Just es -> (Spec ds es obj, mp)
        Nothing -> (Spec ds nubbed obj, mp) )

    where

    choices :: [Expr] -> [[Expr]]
    choices ts =
        -- TODO try using orderedSubsequences
        let ways = sortBy (comparing length) . (init . subsequences) $ ts
        in  ways

    process :: [[Expr]] -> RRR (Timed (Maybe [Expr]))
    process []     = return $ Continue $ Nothing
    process (x:xs) = timedSpec (Spec ds x obj) mp f g
        where

          f (Just r) = do
            recordResult r
            return $ Just x

          f Nothing  = return Nothing

          g (Just r) = do
            recordResult r
            return . Continue . Just $ x

          g Nothing = process xs

-- | We can a given to a find since it is easier to work with
givensToFinds :: (Spec,  Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
givensToFinds d@(_, Nothing)          = return $ Continue $ d
givensToFinds d@(_,(Just (Point []))) = return $ Continue $ d

givensToFinds org@(sp@(Spec ods es obj),(Just point@(Point ps)) ) = do
  let could = unusedDomains sp
  let unused = [ n | ((Name n), _) <- ps, ( n `elem` could ) ]
  let unusedGivens = [ n | (Name n,_) <- ps, n `elem` unused ]

  res <- process (choices unusedGivens)
  forM res $ \x -> case x of
    Nothing        -> return org
    Just (fds, fp) -> return $ ((Spec fds es obj), emptyPointToNothing fp)


  where
  ds = "unused" `M.delete` ods

  process :: [(Domains,Point)]-> RRR (Timed (Maybe (Domains, Maybe Point)))
  process []     = return $ Continue $ Nothing
  process ((y,p):xs) = timedSpec (Spec y es obj) (Just p) f g
      where

        f (Just r) = do
          recordResult r
          return $ Just (y, Just  p)

        f _  =  return $ Nothing

        g (Just r) = do
          recordResult r
          return . Continue . Just $ (y,Just p)

        g _ = process xs

  choices :: [Text] -> [(Domains, Point)]
  choices ts =
      -- res would be [ [], [a], [b], [a,b],  ... ]
      let ways  = orderedSubsequences $ ts
          parts = map (\wy -> (wy, [ n | n <- ts,  n `elem` ts ])) ways
      in map convert parts

  convert ::  ([Text], [Text])
          -> (Domains, Point)
  convert (toFind,_) =
    let nds :: Domains = foldr (\k m-> M.adjustWithKey f k m) ds toFind
    in (nds, coordinateGivens1 nds point)

    where
      f _ (ix,Givenn d) = (ix,Findd d)
      f _ val           = val

simplyFinds:: (Spec,  Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
simplyFinds d@((Spec ds _ _), _) = simplyDomain d org others wrapper doSpec
  where
    org     = [ ((name,ix),val) | (name, (ix, Findd val)) <- M.toList ds ]
    others  = M.filter isGiven ds
    wrapper = Findd
    doSpec :: SpecRunner
    doSpec sp mp f g = timedSpec sp mp (f mp) (g mp)


simplyGivens:: (Spec,  Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
simplyGivens d@(_,Nothing) = return $ Continue d
simplyGivens d@((Spec ds _ _), _) = simplyDomain d org others wrapper doSpec
  where
    org     = [ ((name,ix),val) | (name, (ix, Givenn val)) <- M.toList ds ]
    others  = M.filter isFind ds
    wrapper = Givenn
    doSpec :: SpecRunner
    doSpec  = validateThenRunSpec

simplyDomain :: (Spec,  Maybe Point)
             -> [((Text, Int), Domain () Expr)]
             -> M.Map Text (Int, GF)
             -> (Domain () Expr -> GF)
             -> SpecRunner
             -> RRR (Timed (Spec,  Maybe Point))
simplyDomain d@(Spec _ es obj, mp) org others wrapper doSpec = do
  domsToDo <- doDoms org
  -- liftIO $ putStrLn . show . prettyArr $ map prettyArr domsToDo
  fin <- process1 [ dd |  dd <- domsToDo, dd /= org]

  case timedExtract fin of
    ([],_) -> return $ Continue d
    _      -> return $ flip fmap fin $ \(x,nPoint) -> (Spec (toDoms x) es obj, nPoint)

  where

  toDoms :: [((Text,Int), Domain () Expr)] -> Domains
  toDoms vals =ensureAFind $  (M.fromList $ map trans vals) `M.union` others
  trans ((te,ix),dom) = (te, (ix, wrapper dom))

  doDoms :: [( (Text,Int), Domain () Expr)] -> RRR [[((Text,Int),Domain () Expr)]]
  doDoms [] = docError [ "No domains in reduce:simplyDomain"
                       , nn "spec" d
                       ]
  doDoms ((tx,x):xs) = do
    rx <- runReduce x >>= return . ensureElem x
    rs <- forM xs $ \(t,y) -> do
            ys <- runReduce y >>= return . ensureElem y
            pure $ map (t,) ys
    return $ map (tx,) rx : rs


  process1 :: [[((Text,Int),Domain () Expr)]]
           -> RRR (Timed ([((Text,Int),Domain () Expr)],Maybe Point) )

  process1 []              = return . Continue $ ([], mp)
  process1 xs | (== []) xs = return . Continue $ ([],mp)

  process1 xs | all (singleElem) xs = do
    let fixed = map (headNote "process simplyDomains") xs
    let f newPoint (Just r) = do
            recordResult r
            return (fixed, newPoint)
        f newPoint _ = return ([], newPoint)
    let g newPoint res = do
          x <- f newPoint res
          return $ Continue $ x

    doSpec (Spec (toDoms fixed) es obj) mp f g

  process1 xs = do
    fixed <- next xs
    let
      f newPoint (Just r) = do
        recordResult r
        return (fixed,newPoint)

      f newPoint _  = return ([],newPoint)

      g newPoint (Just r) = do
        recordResult r
        return $ Continue  $ (fixed,newPoint)

      g _ _ = removeNext xs >>= process1

    doSpec (Spec (toDoms fixed) es obj) mp f g


simplyConstraints :: (Spec,  Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
simplyConstraints d@(Spec _ [] _, _)    = return $ Continue $ d
simplyConstraints d@(Spec ds es obj, mp) = do
  choices <- doConstraints es
  fin     <- process1 choices

  if (timedExtract fin) == [] then
      return $ Continue d
  else
      -- return $ fmap (const $ Spec ds (timedExtract fin) obj) fin
      return $ flip fmap fin $ \x -> ( Spec ds x obj  , mp)

  where

  doConstraints :: [Expr] -> RRR [[Expr]]
  doConstraints [] = docError [ "No constraints in reduce:simplyConstraints" ]
  doConstraints (x:xs) = do
    rx <- runReduce  x >>= return . ensureElem x
    rs <- forM xs $ \y -> do
            ys <- runReduce y >>= return . ensureElem y
            pure ys
    return $ rx : rs

  process1 :: [[Expr]] -> RRR (Timed [Expr])

  process1 []              = return . Continue $ []
  process1 xs | (== []) xs = return . Continue $ []

  process1 xs | all (singleElem) xs = do
    let fixed = map (headNote "process simplyFinds") xs
    let f (Just r) = do
            recordResult r
            return fixed
        f _ = return []
    timedSpec (Spec ds fixed obj) mp f (fmap Continue . f)

  process1 xs = do
    fixed <- next xs
    let
      f (Just r) = do
        recordResult r
        return fixed

      f _  = return []

      g (Just r) = do
        recordResult r
        return $ Continue  $ fixed

      g _ = removeNext xs >>= process1

    timedSpec (Spec ds fixed obj) mp f g


-- | Try treating the eprime as a essence spec, and see if still has an error
eprimeAsSpec :: (Spec,  Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
eprimeAsSpec start@(nsp,mp) = do
  config <- gets rconfig
  process config

  where
  process RConfig{..}| oErrKind_ `elem` [RefineCompact_, RefineRandom_ , RefineAll_] = do
    noteFormat "eprimeAsSpec" ["Skipping since this is a refinement error"]
    return (Continue start)


  process _ = do
    gets mostReduced_ >>= \case
      Nothing -> do
        noteFormat1 "eprimeAsSpec" "No most reduced refine original spec first"
        originalInDb >>= \case
          Just Passing{}  -> return (Continue start)
          Just StoredError{} -> rrError "StoredResult in runSpec" []
          Just (OurError (ErrData{specDir})) -> processRefinement specDir
          Nothing -> do
            ((errData,_),_) <-  get >>= \rr -> (flip runStateT) rr (return nsp
                                    >>=  (flip checkForError) mp
                                    )
            case errData of
              Nothing               -> return (Continue start)
              Just ErrData{specDir} -> do
                res <- processRefinement specDir
                liftIO $ removeDirectoryRecursive (specDir)
                return res

      Just ErrData{specDir} -> processRefinement specDir

    where
      processRefinement specDir = do
        files <- liftIO $  getDirectoryContents  specDir
        case [ h | h <- files, takeExtension h == ".eprime" ] of
          [ele] -> do
            readEprimeAsEssence (specDir </> ele) >>= \case
              Nothing  -> do
                noteFormat "eprimeAsSpec readEprimeAsEssence"
                          ["readEprimeAsEssence erred", nn "eprime" ele]
                return (Continue start)
              (Just x) -> do
                may <- runMaybeT $  fromConjure x
                case may of
                  Nothing -> do
                    noteFormat "eprimeAsSpec fromConjure"
                              ["fromConjure erred", nn "val" x]
                    return (Continue start)
                  (Just eprimeSpec) -> do
                    -- curState <- get
                    -- noteFormat "eprimeAsSpec curState" [pretty curState]
                    case mp of
                      Just{} -> do
                        let paramPath = specDir </> ele ++ "-param"
                        mayEParam <- readPointMay paramPath
                        case mayEParam of
                          Nothing -> do
                            noteFormat "eprimeAsSpec"
                                      [nn "no readable param for " ele]
                            return (Continue start)
                          eParam@Just{} ->do
                            noteFormat "eprimeAsSpec"
                                      [nn "Trying" ele, nn "with" (ele ++ "-param")]
                            timedCompactSpec eprimeSpec eParam f (g (eprimeSpec, eParam))

                      Nothing -> do
                        noteFormat "eprimeAsSpec"
                                  [nn "Trying "  ele]
                        timedCompactSpec eprimeSpec Nothing f (g (eprimeSpec, Nothing))

          -- TODO handle multiple eprimes
          _  -> do
            noteFormat "eprimeAsSpec NotDone" ["multiple eprimes not supported"]
            return (Continue start)

      f x = do -- No time to reduce the eprime
        noteFormat "eprimeAsSpec noTimeLeft" [pretty x]
        return $ start

      g _ Nothing = do -- No Error occured, should not happen?
        noteFormat "eprimeAsSpec NoError" []
        return $ Continue $ start

      g sp_p (Just r) = do
        noteFormat "eprimeAsSpec SameError" [pretty r]
        recordResult r
        loopToFixed False sp_p


      originalInDb :: (MonadDB m, MonadState RState m, MonadIO m)
                   => m (Maybe RunResult)
      originalInDb = do
        RConfig{oErrKind_, oErrStatus_} <- gets rconfig
        checkDB oErrKind_ oErrStatus_ nsp mp


ensureAFind :: Domains -> Domains
ensureAFind ds | M.null ds = M.insert ("unused") (1, Findd DomainBool) ds
ensureAFind ds =case  any isFind (M.elems ds) of
                    True  -> ds
                    False -> M.insert ("unused") (1, Findd DomainBool) ds

isGiven :: (Int,GF) -> Bool
isGiven (_, Givenn{}) = True
isGiven _             = False

isFind :: (Int,GF) -> Bool
isFind (_, Findd{})  = True
isFind _             = False

coordinateGivens :: Domains -> Maybe Point -> Maybe Point
coordinateGivens _ Nothing = Nothing
coordinateGivens ds (Just p) = Just $ coordinateGivens1 ds p

coordinateGivens1 :: Domains -> Point -> Point
coordinateGivens1 ds p =
    let keep = M.filter isGiven ds
    in onlyNames (M.keysSet keep) p

emptyPointToNothing :: Maybe Point -> Maybe Point
emptyPointToNothing (Just (Point [])) = Nothing
emptyPointToNothing xs = xs


-- List functions

-- | Fix the next Elem
next :: [[x]] -> RRR [x]
next esR = return $ map pickFirst esR

  where
  pickFirst []    = lineError $line ["pickfirst empty"]
  pickFirst [x]   = x
  pickFirst (x:_) = x

removeNext :: [[a]] -> RRR [[a]]
removeNext []                     = rrError "removeNext empty" []
removeNext xs | any null xs       = rrError "removeNext sub empty" []
removeNext xs | all singleElem xs = return xs

removeNext ([]:xs )    = ([]:)   <$> removeNext xs
removeNext ([x]:xs)    = ([x]:)  <$> removeNext xs
removeNext ((_:fs):xs) = return $ fs:xs


ensureElem :: a -> [a] -> [a]
ensureElem z []  = [z]
ensureElem _ xs  = xs

singleElem :: [a] -> Bool
singleElem [_] = True
singleElem _   = False


recordResult :: ErrData -> RRR ()
recordResult err = do
  modify $ \st -> st{ mostReduced_=Just err
                    , mostReducedChoices_=Just (choices err) }
  return ()

-- |  Run the computation if there is time left
con :: Doc
    -> ( (Spec, Maybe Point) -> RRR (Timed (Spec, Maybe Point) ))
    -> Timed (Spec, Maybe Point) -> RRR (Timed (Spec, Maybe Point))
con tx _ (NoTimeLeft s) = do
    noteFormat ("@" <+> tx <+> "Start/NoTimeLeft") []
    return $ NoTimeLeft s

con tx f (Continue d) = do
    noteFormat ("@" <+> tx <+> "Start") []

    (newRes) <- f d
    endState <- get
    noteFormat ("@" <+> tx <+> "End") (prettyTimedResult newRes)
    RConfig{printState_} <- gets rconfig
    when printState_ $ noteFormat ("@" <+> tx <+> "EndState") [pretty endState]
    note ""

    return newRes

prettyTimedResult :: Timed (Spec, Maybe Point) -> [Doc]
prettyTimedResult t =
  let (sp,mp ) = timedExtract t
  in [ pretty sp
     , maybe "" (("param" <+>) . pretty) (mp)]

type SpecRunner = forall a
    .  Spec -> Maybe Point
    -> (Maybe Point -> Maybe ErrData -> RRR a)          -- No time left
    -> (Maybe Point -> Maybe ErrData -> RRR (Timed a))  -- Time left
    -> RRR (Timed a)

validateThenRunSpec :: SpecRunner
validateThenRunSpec spec Nothing f g = timedSpec spec Nothing (f Nothing) (g Nothing)
validateThenRunSpec spec ojp@(Just ops) f g  = do
  pointVaild <- liftIO $ ignoreNotes $ validatePoint1 spec ops
  case pointVaild of
    True  ->  do
      noteFormat "PointStillValid" [nn "Spec" spec, nn "point" ops]
      timedSpec spec ojp (f ojp) (g ojp)
    False -> do
      noteFormat "PointInvalided" [nn "Spec" spec, nn "point" ops]
      generatePoint spec >>= \case
        Nothing -> do
          noteFormat "mkPointFailed" ["Failed to create a new point", ""]
          g (Just ops) Nothing
        Just p  -> do
          noteFormat "mkPoint" [nn "Created" p, ""]
          let jp = Just p
          timedSpec spec jp (f jp) (g jp)
