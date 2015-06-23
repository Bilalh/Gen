{-# LANGUAGE TupleSections, ViewPatterns, KindSignatures #-}
module Gen.Reduce.Reduce where

import Conjure.Language.Domain
import Gen.Helpers.Log
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.RunResult
import Gen.Reduce.Data
import Gen.Reduce.QuanToComp    (quanToComp)
import Gen.Reduce.Reduction
import Gen.Reduce.Runner
import Gen.Reduce.UnusedDomains

import qualified Data.Map as M
import qualified Text.PrettyPrint as Pr

reduceMain :: Bool -> RState -> IO RState
reduceMain check rr = do
  let base = (specDir_ . rconfig) rr
      fp   =  base </> "spec.spec.json"

  sp_ <- readFromJSON fp
  -- Remove quantification
  sp <-  quanToComp sp_
  -- groomPrint sp
  errOccurs <- case check of
                 False -> return (True, rr)
                 True -> (flip runStateT) rr (return sp
                           >>= noteMsg "Checking if error still occurs"
                           >>= runSpec
                           >>= \case
                                  (Just ErrData{..}, _) -> do
                                    liftIO $ removeDirectoryRecursive (specDir)
                                    return True

                                  _ -> return False
                       )
  case errOccurs of
    (False, _) -> do
        putStrLn "Spec has no error with the given settings, not reducing"
        return rr
    (True, _) -> do
      -- noteFormat "StateStateStart" [pretty rr]
      (sfin,state) <- (flip runStateT) rr $
          return sp
          >>= doReductions
          >>= \ret -> get >>= \g -> addLog "FinalState" [pretty g] >> return ret


      noteFormat "StateState" [pretty state]
      noteFormat "Start" [pretty sp]

      end <- case sfin of
               (Continue x)   -> return $  Just x
               NoTimeLeft{} -> do
                  case mostReduced_ state of
                    Nothing -> return Nothing
                    Just (ErrData{..})  ->
                        readFromJSON (specDir </> "spec.spec.json")

      let end2 = case end of
                   Nothing -> Nothing
                   Just x  -> if hash x == hash sp  then
                                  Nothing
                              else
                                  Just x

      noteFormat "Final" [pretty end2]

      return (state)

  where
  noteMsg tx s = do
      noteFormat ("@" <+> tx) []
      return s


doReductions :: Spec -> RR (Timed Spec)
doReductions start =
    return (Continue start)
    -- >>= con "tryRemoveConstraints" tryRemoveConstraints
    -- >>= con "removeObjective"      removeObjective
    -- >>= con "removeUnusedDomains"  removeUnusedDomains
    >>= con "simplyDomains"        simplyDomains
    -- >>= con "removeConstraints"    removeConstraints
    -- >>= con "simplyConstraints"    simplyConstraints
    -- >>= con "loopToFixed"          loopToFixed

con :: forall (m :: * -> *) a . (MonadIO m, Pretty a)
    => Doc
    -> (a -> m (Timed a))
    -> Timed a -> m (Timed a)
con tx _ (NoTimeLeft s) = do
    noteFormat ("@" <+> tx <+> "Start/NoTimeLeft") []
    return $ NoTimeLeft s

con tx f (Continue s) = do
    noteFormat ("@" <+> tx <+> "Start") []

    newSp <- f s
    noteFormat ("@" <+> tx <+> "End") [pretty newSp]

    return newSp


loopToFixed :: Spec -> RR (Timed Spec)
loopToFixed start = do
  noteFormat ("@" <+> "loopToFixed") []
  res <-  return (Continue start)
      >>= con "tryRemoveConstraints" tryRemoveConstraints
      >>= con "removeObjective"      removeObjective
      >>= con "removeUnusedDomains"  removeUnusedDomains
      >>= con "simplyDomains"        simplyDomains
      >>= con "removeConstraints"    removeConstraints
      >>= con "simplyConstraints"    simplyConstraints
  case res of
    (NoTimeLeft end) -> return $ NoTimeLeft end
    (Continue cur)   -> do
      if hash start == hash cur then
          return $ Continue start
      else
          loopToFixed cur


tryRemoveConstraints :: Spec -> RR (Timed Spec)
tryRemoveConstraints sp@(Spec _ [] _ )  = return . Continue $ sp
tryRemoveConstraints sp@(Spec ds _ obj) = do
  timedSpec (Spec ds [] obj) f (  fmap Continue . f )


  where
    f (Just r) = do
      recordResult r
      return $ Spec ds [] obj

    f _ = return sp

removeObjective :: Spec -> RR (Timed Spec)
removeObjective sp@(Spec _ _ Nothing)  = return . Continue $ sp
removeObjective sp@(Spec ds es Just{}) =
  timedSpec (Spec ds es Nothing ) f (fmap Continue . f)

  where
    f (Just r) = do
      recordResult r
      return $ Spec ds es Nothing

    f _ = return sp

removeUnusedDomains :: Spec -> RR (Timed Spec)
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

    process :: [Domains]-> RR (Timed (Maybe Domains))
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


removeConstraints :: Spec -> RR (Timed Spec)
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

    process :: [[Expr]] -> RR (Timed (Maybe [Expr]))
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

    -- process ts = error . show . prettyBrackets . vcat $ map (prettyBrackets .  vcat . map pretty) ts


simplyDomains :: Spec -> RR (Timed Spec)
simplyDomains sp@(Spec ds es obj) = do
  --FIXME assumes all finds
  domsToDo <- doDoms ( map (second domOfGF) . M.toList $ ds)
  addLog "Got Domains" []
  liftIO $ putStrLn . show . prettyArr $ map prettyArr domsToDo
  fin <- process1 domsToDo
  -- fin    <- process [ ( map (second domOfGF) . M.toList $ ds) ]
  addLog "finished processing" []

  if (timedExtract fin) == [] then
      return $ Continue sp
  else
      return $ fmap (const $ Spec (toDoms $ timedExtract fin) es obj) fin

  where
  toDoms :: [(Text, Domain () Expr)] -> Domains
  toDoms = M.fromList . map (second Findd)

  --  | Fix the next Domain
  next :: [[(Text,Domain () Expr)]] -> RR [(Text,Domain () Expr)]
  next esR = return $ map pickFirst esR

    where
    pickFirst []    = error "pickfirst empty"
    pickFirst [x]   = x
    pickFirst (x:_) = x

  doDoms :: [(Text,Domain () Expr)] -> RR [[(Text,Domain () Expr)]]
  doDoms [] = docError [ "No domains in reduce:simplyDomains" ]
  doDoms ((tx,x):xs) = do
    rx <- runReduce sp x
    rs <- mapM (\(t,y) -> do{ys <- runReduce sp y; pure $ (t,y) : map (t,) ys }) xs
    return $ map (tx,) rx : rs


  process1 :: [[(Text,Domain () Expr)]] -> RR (Timed [(Text,Domain () Expr)])

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



simplyConstraints :: Spec -> RR (Timed Spec)
simplyConstraints sp@(Spec ds es obj) = do
    csToDo <- doConstraints es
    addLog "Got Constraints" []
    fin    <- process csToDo
    addLog "finished processing" []

    if (timedExtract fin) == [] then
        let f (Just r) = do
              recordResult r
              return (Spec ds [] obj)
            f _ = return $ Spec ds es obj
        in
        timedSpec (Spec ds [] obj)  f (fmap Continue . f)
    else
        return $ fmap (const $ Spec ds (timedExtract fin) obj) fin

    where
    process :: [[Expr]] -> RR (Timed [Expr])

    -- cannot simply any futher
    process xs | any (== []) xs = return . Continue $ []

    process xs | all (singleElem) xs = do
        addLog "processsingleElem" []
        let fix = map head xs
        let f (Just r) = do
              recordResult r
              return fix
            f _ = return []
        timedSpec (Spec ds fix obj) f (fmap Continue . f)

    process esR = do
        addLog "process esR" []
        fix <- choose esR

        let
            f (Just r) = do
              recordResult r
              return fix

            f _  = return fix

            g (Just r) = do
              recordResult r
              innerToDo <- doConstraints fix
              inner     <- process innerToDo
              if (timedExtract inner) == [] then
                  return $ fmap (const fix) inner
              else
                  return inner

            g _  = removeNext esR >>= process

        timedSpec (Spec ds fix obj) f g

    -- Fix the next constraint
    choose :: [[Expr]] -> RR [Expr]
    choose esR = do
        addLog "choose esR" []
        return $ map pickFirst esR

        where
        pickFirst []    = error "pickfirst empty"
        pickFirst [x]   = x
        pickFirst (x:_) = x

    -- Keep the orginal exprs apart from the first
    doConstraints :: [Expr] -> RR [[Expr]]
    doConstraints [] = return [[]]
    doConstraints (x:xs) = do
        addLog "doConstraints xs" []
        rx <- runReduce sp x
        rs <- mapM (\y -> do { ys <- runReduce sp y; return $ y : ys } ) xs
        return $ rx : rs

removeNext :: [[a]] -> RR [[a]]
removeNext []                     = rrError "removeNext empty" []
removeNext xs | any null xs       = rrError "removeNext sub empty" []
removeNext xs | all singleElem xs = return xs

removeNext ([]:xs )    = ([]:)   <$> removeNext xs
removeNext ([x]:xs)    = ([x]:)  <$> removeNext xs
removeNext ((_:fs):xs) = return $ fs:xs



tailR :: [a] -> [a]
tailR []     = error "tailR empty list"
tailR [x]    = [x]
tailR (_:xs) = xs

singleElem :: [a] -> Bool
singleElem [_] = True
singleElem _   = False


recordResult :: ErrData -> RR ()
recordResult err = do
  modify $ \st -> st{ mostReduced_=Just err
                    , mostReducedChoices_=Just (choices err) }
  return ()
