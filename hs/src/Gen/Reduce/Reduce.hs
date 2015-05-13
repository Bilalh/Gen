module Gen.Reduce.Reduce where

import Gen.Reduce.Data
import Gen.Reduce.Runner
import Gen.IO.Formats
import Gen.Reduce.Reduction
import Gen.Reduce.UnusedDomains
import Gen.Imports
import Conjure.Language.Domain
import Gen.Reduce.QuanToComp(quanToComp)
import Gen.Helpers.Log

import qualified Data.Map as M

reduceMain :: Bool -> RState -> IO RState
reduceMain check rr = do
  let base = specDir_ rr
      fp   =  base </> "spec.spec.json"

  sp_ <- readFromJSON fp
  -- Remove quantification
  sp <-  quanToComp sp_

  errOccurs <- case check of
                 False -> return (Just $never, rr)
                 True -> (flip runStateT) rr (return sp
                           >>= noteMsg "Checking if error still occurs"
                           >>= runSpec
                           >>= \case
                                  (Nothing,_) -> return Nothing
                                  (Just x, _) -> do
                                    liftIO $ removeDirectoryRecursive (resDirectory_ x)
                                    return (Just x)

                       )
  case errOccurs of
    (Nothing, _) -> do
        putStrLn "Spec has no error with the given settings, not reducing"
        return rr
    _ -> do
      noteFormat "StateStateStart" [pretty rr]
      (sfin,state) <- (flip runStateT) rr $
          return sp
          >>= doReductions
          >>= \ret -> get >>= \g -> addLog "FinalState" [pretty g] >> return ret


      noteFormat "StateState" [pretty state]
      noteFormat "Start" [pretty sp]
      noteFormat "Final" [pretty sfin]

      return (state)

  where
  noteMsg tx s = do
      noteFormat ("@" <+> tx) []
      return s


doReductions :: Spec -> RR (Timed Spec)
doReductions start =
    return (Continue start)
    >>= con "tryRemoveConstraints" tryRemoveConstraints
    >>= con "removeUnusedDomains"  removeUnusedDomains
    >>= con "removeConstraints"    removeConstraints
    >>= con "simplyConstraints"    simplyConstraints


  where
    con tx _ (NoTimeLeft s) = do
        noteFormat ("@" <+> tx <+> "Start/NoTimeLeft") []
        return $ NoTimeLeft s

    con tx f (Continue s) = do
        noteFormat ("@" <+> tx <+> "Start") []

        newSp <- f s
        noteFormat ("@" <+> tx <+> "End") [pretty newSp]

        return newSp


tryRemoveConstraints :: Spec -> RR (Timed Spec)
tryRemoveConstraints sp@(Spec _ [] _ )  = return . Continue $ sp
tryRemoveConstraints sp@(Spec ds _ obj) = do
  timedSpec (Spec ds [] obj) f (  fmap Continue . f )


  where
    f (Just r) = do
      recordResult r
      return $ Spec ds [] obj

    f Nothing = return sp

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

          f Nothing  = return $ Just y
          f (Just r) = do
            recordResult r
            return $ Just y

          g Nothing = process xs
          g (Just r) = do
            recordResult r
            return . Continue . Just $ y



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
          f Nothing  = return $ Just x
          f (Just r) = do
            recordResult r
            return $ Just x

          g Nothing = process xs
          g (Just r) = do
            recordResult r
            return . Continue . Just $ x

    -- process ts = error . show . prettyBrackets . vcat $ map (prettyBrackets .  vcat . map pretty) ts

simplyConstraints :: Spec -> RR (Timed Spec)
simplyConstraints sp@(Spec ds es obj) = do
    csToDo <- doConstraints es
    addLog "Got Constraints" []
    fin    <- process csToDo
    addLog "finished processing" []

    --FIXME check
    if (timedExtract fin) == [] then
        let f (Just r) = do
              recordResult r
              return (Spec ds [] obj)
            f Nothing = return $ Spec ds es obj
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
            f Nothing = return []
        timedSpec (Spec ds fix obj) f (fmap Continue . f)

    process esR = do
        addLog "process esR" []
        fix <- choose esR

        let f Nothing  = return fix
            f (Just r) = do
              recordResult r
              return fix

            g Nothing   = removeNext esR >>= process
            g (Just r)  = do
              recordResult r
              innerToDo <- doConstraints fix
              inner     <- process innerToDo
              if (timedExtract inner) == [] then
                  return $ fmap (const fix) inner
              else
                  return inner

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
    removeNext xs | all singleElem xs = return xs
    removeNext xs | any null xs       = rrError "removeNext sub empty" []

    removeNext ([x]:xs)    = ([x]:)  <$> removeNext xs
    removeNext ((_:fs):xs) = return $ fs:xs
    removeNext (x:xs )     = (x:) <$> removeNext xs



tailR :: [a] -> [a]
tailR []     = error "tailR empty list"
tailR [x]    = [x]
tailR (_:xs) = xs

singleElem :: [a] -> Bool
singleElem [_] = True
singleElem _   = False


recordResult :: RunResult -> RR ()
recordResult r= do
  modify $ \st -> st{mostReduced_=Just r, mostReducedChoices_=Just (resErrChoices_ r) }
  return ()
