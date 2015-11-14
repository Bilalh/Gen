{-# LANGUAGE KindSignatures, Rank2Types, TupleSections #-}
module Gen.Reduce.Reduce where

import Conjure.Language.Domain
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.RunResult
import Gen.IO.ToolchainData
import Gen.Reduce.Data
import Gen.Reduce.QuanToComp    (quanToComp)
import Gen.Reduce.Random
import Gen.Reduce.Reduction
import Gen.Reduce.Runner
import Gen.Reduce.UnusedDomains
import System.FilePath          (takeExtension)

import qualified Data.Map as M


reduceMain :: (MonadIO m, MonadLog m, RndGen m) => Bool -> RState -> m RState
reduceMain check rr = do
  let base = (specDir_ . rconfig) rr
      fp   =  base </> "spec.spec.json"

  sp_ <- liftIO $ readFromJSON fp
  -- Remove quantification
  sp <-  liftIO $ quanToComp sp_
  (errOccurs,_) <- case check of
                 False -> return (True, rr)
                 True -> (flip runStateT) rr (return sp
                           >>= noteMsg "Checking if error still occurs"
                           >>= (flip runSpec) (param_ rr)
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
          return sp
          >>= doReductions
          >>= \ret -> get >>= \g -> addLog "FinalState" [pretty g] >> return ret


      noteFormat "FinalState" [pretty state]
      noteFormat "Start" [pretty sp]

      end <- case sfin of
               (Continue x)   -> return $  Just x
               NoTimeLeft{} -> do
                  case mostReduced_ state of
                    Nothing -> return Nothing
                    Just (ErrData{..})  ->
                        liftIO $ readFromJSON (specDir </> "spec.spec.json")

      let end2 = case end of
                   Nothing -> Nothing
                   Just x  -> if hash x == hash sp  then
                                  Nothing
                              else
                                  Just x

      noteFormat "Final" [pretty end2]

      return (state)


noteMsg :: MonadIO m => Doc -> b -> m b
noteMsg tx s = do
    noteFormat ("@" <+> tx) []
    return s


doReductions :: Spec -> RRR (Timed Spec)
doReductions start =
    return (Continue start)
    >>= con "tryRemoveConstraints" tryRemoveConstraints
    >>= con "removeObjective"      removeObjective
    >>= con "removeUnusedDomains"  removeUnusedDomains
    >>= con "removeConstraints"    removeConstraints
    >>= con "removeUnusedDomains"  removeUnusedDomains
    >>= con "simplyDomains"        simplyDomains
    >>= con "simplyConstraints"    simplyConstraints
    >>= con "loopToFixed"          loopToFixed
    >>= con "eprimeAsSpec"         eprimeAsSpec


eprimeAsSpec :: Spec -> RRR (Timed Spec)
eprimeAsSpec start = do
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

                    timedCompactSpec eprimeSpec f (g eprimeSpec)
                    -- docError [nn "res" res]

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

      g sp (Just r) = do
        liftIO $ noteFormat "eprimeAsSpec SameError" [pretty r]
        recordResult r
        loopToFixed sp



loopToFixed :: Spec -> RRR (Timed Spec)
loopToFixed start = do
  noteFormat ("@" <+> "loopToFixed") []
  res <-  return (Continue start)
      >>= con "removeObjective"      removeObjective
      >>= con "removeConstraints"    removeConstraints
      >>= con "removeUnusedDomains"  removeUnusedDomains
      >>= con "simplyDomains"        simplyDomains
      >>= con "simplyConstraints"    simplyConstraints
  case res of
    (NoTimeLeft end) -> return $ NoTimeLeft end
    (Continue cur)   -> do
      if hash start == hash cur then
          return $ Continue start
      else
          loopToFixed cur


tryRemoveConstraints :: Spec -> RRR (Timed Spec)
tryRemoveConstraints sp@(Spec _ [] _ )  = return . Continue $ sp
tryRemoveConstraints sp@(Spec ds _ obj) = do
  timedSpec (Spec ds [] obj) f (  fmap Continue . f )

  where
    f (Just r) = do
      recordResult r
      return $ Spec ds [] obj

    f _ = return sp

removeObjective :: Spec -> RRR (Timed Spec)
removeObjective sp@(Spec _ _ Nothing)  = return . Continue $ sp
removeObjective sp@(Spec ds es Just{}) =
  timedSpec (Spec ds es Nothing ) f (fmap Continue . f)

  where
    f (Just r) = do
      recordResult r
      return $ Spec ds es Nothing

    f _ = return sp

removeUnusedDomains :: Spec -> RRR (Timed Spec)
removeUnusedDomains sp@(Spec ods es obj) = do
    let unusedNames = unusedDomains sp

    process (choices ods unusedNames) >>= return . fmap (\x -> case x of
          Just ds -> (Spec ds es obj)
          Nothing -> (Spec ods es obj))

    where
    choices :: Domains -> [Text] -> [Domains]
    choices ds ts =
        -- remove [] and reversing to get largest first
        -- meaning res would be [ [a], [b], [a,b],  ... ]
        let ways = reverse . tail . sortBy (comparing length) . subsequences $ ts
            res = fmap (\wy -> M.filterWithKey (\k _ -> k `notElem` wy) ds ) ways
        in res

    process :: [Domains]-> RRR (Timed (Maybe Domains))
    process []     = return $ Continue $ Nothing
    process (x:xs) = timedSpec (Spec y es obj) f g
        where
          y = ensureADomain x

          f (Just r) = do
            recordResult r
            return $ Just y

          f _  =  return $ Just y

          g (Just r) = do
            recordResult r
            return . Continue . Just $ y

          g _ = process xs


    ensureADomain :: Domains -> Domains
    ensureADomain ds | M.null ds = M.insert ("unused") (Findd DomainBool) ds
    ensureADomain ds = ds


removeConstraints :: Spec -> RRR (Timed Spec)
removeConstraints (Spec ds oes obj) = do
    let nubbed = nub2 oes
    process (choices nubbed) >>= return . fmap (\x -> case x of
        Just es -> Spec ds es obj
        Nothing -> Spec ds nubbed obj )

    where

    choices :: [Expr] -> [[Expr]]
    choices ts =
        let ways = sortBy (comparing length) . (init . subsequences) $ ts
        in  ways

    process :: [[Expr]] -> RRR (Timed (Maybe [Expr]))
    process []     = return $ Continue $ Nothing
    process (x:xs) = timedSpec (Spec ds x obj) f g
        where

          f (Just r) = do
            recordResult r
            return $ Just x

          f _  = return $ Just x

          g (Just r) = do
            recordResult r
            return . Continue . Just $ x

          g _ = process xs


simplyDomains :: Spec -> RRR (Timed Spec)
simplyDomains sp@(Spec ds es obj) = do
  let org = [ (name,val) | (name, Findd val) <- M.toList ds ]
  domsToDo <- doDoms org
  liftIO $ putStrLn . show . prettyArr $ map prettyArr domsToDo
  fin <- process1 [ dd |  dd <- domsToDo, dd /= org]

  if (timedExtract fin) == [] then
      return $ Continue sp
  else
      return $ fmap (const $ Spec (toDoms $ timedExtract fin) es obj) fin

  where
  givens = M.filter isGiven ds where
    isGiven Givenn{} = True
    isGiven _        = False
  toDoms :: [(Text, Domain () Expr)] -> Domains
  toDoms vals = (M.fromList $ map (second Findd) vals) `M.union` givens

  doDoms :: [(Text,Domain () Expr)] -> RRR [[(Text,Domain () Expr)]]
  doDoms [] = docError [ "No domains in reduce:simplyDomains" ]
  doDoms ((tx,x):xs) = do
    rx <- runReduce sp x >>= return . ensureElem x
    rs <- forM xs $ \(t,y) -> do
            ys <- runReduce sp y >>= return . ensureElem y
            pure $ map (t,) ys
    return $ map (tx,) rx : rs

  process1 :: [[(Text,Domain () Expr)]] -> RRR (Timed [(Text,Domain () Expr)])

  process1 []              = return . Continue $ []
  process1 xs | (== []) xs = return . Continue $ []

  process1 xs | all (singleElem) xs = do
    let fixed = map (headNote "process simplyDomains") xs
    let f (Just r) = do
            recordResult r
            return fixed
        f _ = return []
    timedSpec (Spec (toDoms fixed) es obj) f (fmap Continue . f)

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

    timedSpec (Spec (toDoms fixed) es obj) f g

simplyConstraints :: Spec -> RRR (Timed Spec)
simplyConstraints sp@(Spec _ [] _)    = return $ Continue $ sp
simplyConstraints sp@(Spec ds es obj) = do
  choices <- doConstraints es
  fin     <- process1 choices

  if (timedExtract fin) == [] then
      return $ Continue sp
  else
      return $ fmap (const $ Spec ds (timedExtract fin) obj) fin

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
    let fixed = map (headNote "process simplyDomains") xs
    let f (Just r) = do
            recordResult r
            return fixed
        f _ = return []
    timedSpec (Spec ds fixed obj) f (fmap Continue . f)

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

    timedSpec (Spec ds fixed obj) f g


--  | Fix the next Elem
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
con :: forall a . Pretty a
    => Doc
    -> (a -> RRR (Timed a))
    -> Timed a -> RRR (Timed a)
con tx _ (NoTimeLeft s) = do
    noteFormat ("@" <+> tx <+> "Start/NoTimeLeft") []
    return $ NoTimeLeft s

con tx f (Continue s) = do
    noteFormat ("@" <+> tx <+> "Start") []

    newSp <- f s
    noteFormat ("@" <+> tx <+> "End") [pretty newSp]
    endState <- get
    noteFormat ("@" <+> tx <+> "EndState") [pretty endState]
    liftIO $ putStrLn ""

    return newSp
