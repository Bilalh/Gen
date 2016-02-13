{-# LANGUAGE KindSignatures, Rank2Types, TupleSections #-}
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

import qualified Data.Map as M

reduceMain :: (MonadIO m, MonadLog m, RndGen m) => Bool -> RState -> m RState
reduceMain check rr = do
  let base = (specDir_ . rconfig) rr
      fp   =  base </> "spec.spec.json"


  sp_ <- liftIO $ readFromJSON fp

  -- Remove quantification and enums
  let paramFp_ = base </> "given.param"
  paramFp <- liftIO $ doesFileExist paramFp_  >>= \case
    False -> return Nothing
    True  -> return (Just paramFp_)

  (sp,startParam) <-  liftIO $ deEnum sp_ paramFp

  (errOccurs,_) <- case check of
                 False -> return (True, rr)
                 True -> (flip runStateT) rr (return sp
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
        liftIO $ putStrLn "Spec has no error with the given settings, not reducing"
        return rr
    True -> do
      (sfin,state) <-  (flip runStateT) rr $
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
            Just (nsp,nmp) -> if hash nsp == hash sp && hash startParam == hash nmp then
                         "Nothing"
                       else
                          pretty nsp $$ maybe "" (("param" <+>) . pretty) nmp

      noteFormat "Final" $ [end2]

      return (state)

noteMsg :: MonadIO m => Doc -> b -> m b
noteMsg tx s = do
    noteFormat ("@" <+> tx) []
    return s

-- | The reduction process
doReductions :: (Spec, Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
doReductions start =
    return (Continue start)
    >>= con "tryRemoveConstraints" tryRemoveConstraints
    >>= con "loopToFixed"          loopToFixed
    >>= con "eprimeAsSpec"         eprimeAsSpec


loopToFixed :: (Spec,  Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
loopToFixed start = do
  noteFormat ("@" <+> "loopToFixed") []
  res <-  return (Continue start)
      >>= con "removeObjective"      removeObjective
      >>= con "removeUnusedDomains"  removeUnusedDomains
      >>= con "removeConstraints"    removeConstraints
      >>= con "inlineGivens"         inlineGivens
      >>= con "simplyFinds"          simplyFinds
      >>= con "simplyConstraints"    simplyConstraints
      -- >>= con "simplyGivens"         simplyGivens

  case res of
    (NoTimeLeft end) -> return $ NoTimeLeft end
    (Continue cur)   -> do
      if hash start == hash cur then
          return $ Continue start
      else
          loopToFixed cur


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
        -- remove [] and reversing to get largest first
        -- meaning res would be [ [a], [b], [a,b],  ... ]
        let ways = reverse . tail . sortBy (comparing length) . subsequences $ ts
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
    Just (fsp, fp) -> return (fsp, Just fp)

  where
  -- list of points in decressing length  [(a,_),(b,_),(c,)],  [(a,_),(b,_)]
  givenSeq = reverse . tail . sortBy (comparing length) . subsequences $ ops

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


simplyFinds :: (Spec,  Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
simplyFinds d@(sp@(Spec ds es obj), mp) = do
  let org = [ ((name,ix),val) | (name, (ix, Findd val)) <- M.toList ds ]
  domsToDo <- doDoms org
  liftIO $ putStrLn . show . prettyArr $ map prettyArr domsToDo
  fin <- process1 [ dd |  dd <- domsToDo, dd /= org]

  if (timedExtract fin) == [] then
      return $ Continue d
  else
      return $ flip fmap fin $ \x -> ( Spec (toDoms x) es obj  , mp)

  where
  givens = M.filter isGiven ds where

  toDoms :: [((Text,Int), Domain () Expr)] -> Domains
  toDoms vals =ensureAFind $  (M.fromList $ map trans vals) `M.union` givens
  trans ((te,ix),dom) = (te, (ix, Findd dom))

  doDoms :: [( (Text,Int), Domain () Expr)] -> RRR [[((Text,Int),Domain () Expr)]]
  doDoms [] = docError [ "No domains in reduce:simplyFinds" ]
  doDoms ((tx,x):xs) = do
    rx <- runReduce sp x >>= return . ensureElem x
    rs <- forM xs $ \(t,y) -> do
            ys <- runReduce sp y >>= return . ensureElem y
            pure $ map (t,) ys
    return $ map (tx,) rx : rs

  process1 :: [[((Text,Int),Domain () Expr)]] -> RRR (Timed [((Text,Int),Domain () Expr)])

  process1 []              = return . Continue $ []
  process1 xs | (== []) xs = return . Continue $ []

  process1 xs | all (singleElem) xs = do
    let fixed = map (headNote "process simplyFinds") xs
    let f (Just r) = do
            recordResult r
            return fixed
        f _ = return []
    timedSpec (Spec (toDoms fixed) es obj) mp f (fmap Continue . f)

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

    timedSpec (Spec (toDoms fixed) es obj) mp f g


simplyConstraints :: (Spec,  Maybe Point) -> RRR (Timed (Spec,  Maybe Point))
simplyConstraints d@(Spec _ [] _, _)    = return $ Continue $ d
simplyConstraints d@(sp@(Spec ds es obj), mp) = do
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
    rx <- runReduce sp x >>= return . ensureElem x
    rs <- forM xs $ \y -> do
            ys <- runReduce sp y >>= return . ensureElem y
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
eprimeAsSpec start@(_,mp) = do
  config <- gets rconfig
  process config

  where
  process RConfig{..} |  oErrKind_ `notElem` [Savilerow_]
                      || oErrStatus_ `elem`  [ParseError_] =
    return (Continue start)

  process _ = do
    gets mostReduced_ >>= \case
      Nothing -> return (Continue start)
      Just (ErrData{specDir}) -> do

        files <- liftIO $  getDirectoryContents  specDir
        case [ h | h <- files, takeExtension h == ".eprime" ] of
          [ele] -> do
            readEprimeAsEssence ele >>= \case
              Nothing  -> return (Continue start)
              (Just x) -> do
                may <- runMaybeT $  fromConjure x
                case may of
                  Nothing -> return (Continue start)
                  (Just eprimeSpec) -> do
                    -- curState <- get
                    -- noteFormat "eprimeAsSpec curState" [pretty curState]
                    case mp of
                      Just{} -> do
                        liftIO $ noteFormat "eprimeAsSpec NotDone"
                                   ["eprimeAsSpec not supported with a .param"]
                        return (Continue start)
                      Nothing -> do
                        timedCompactSpec eprimeSpec Nothing f (g (eprimeSpec, Nothing))

          -- TODO handle multiple eprimes
          _  -> do
            liftIO $ noteFormat "eprimeAsSpec NotDone" ["multiple eprimes not supported"]
            return (Continue start)

    where
      f x = do -- No time to reduce the eprime
        liftIO $ noteFormat "eprimeAsSpec noTimeLeft" [pretty x]
        return $ start

      g _ Nothing = do -- No Error occured, should not happen
        liftIO $ noteFormat "eprimeAsSpec NoError" []
        return $ Continue $ start

      g sp_p (Just r) = do
        liftIO $ noteFormat "eprimeAsSpec SameError" [pretty r]
        recordResult r
        loopToFixed sp_p


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
coordinateGivens ds (Just p) =
    let keep = M.filter isGiven ds
    in Just $ onlyNames (M.keysSet keep) p



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
    noteFormat ("@" <+> tx <+> "EndState") [pretty endState]
    liftIO $ putStrLn ""

    return newRes

prettyTimedResult :: Timed (Spec, Maybe Point) -> [Doc]
prettyTimedResult t =
  let (sp,mp ) = timedExtract t
  in [ pretty sp
     , maybe "" (("param" <+>) . pretty) (mp)]
