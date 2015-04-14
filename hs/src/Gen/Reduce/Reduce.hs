module Gen.Reduce.Reduce where

import Gen.Reduce.Data
import Gen.Reduce.Runner
import Gen.IO.Formats
import Gen.Reduce.Reduction
import Gen.Reduce.UnusedDomains
import Gen.Prelude

import Conjure.Language.Domain

import qualified Data.Map as M


reduceMain :: RState -> IO (Spec, RState)
reduceMain rr = do
  let base = specDir_ rr
      fp   =  base </> "spec.spec.json"

  sp <- readFromJSON fp

  (flip runStateT) rr (return sp >>= noteMsg "Checking if error still occurs"  >>= runSpec ) >>= \case
    (Nothing, st) -> do
        putStrLn "Spec has no error with the given settings, not reducing"
        return (sp, st)
    _ -> do
      (sfin,state) <- (flip runStateT) rr $
          return sp
          >>= (note "tryRemoveConstraints") tryRemoveConstraints
          >>= (note "removeUnusedDomains")  removeUnusedDomains
          >>= (note "removeConstraints")    removeConstraints
          >>= (note "simplyConstraints")    simplyConstraints
          >>= (note "removeUnusedDomains")  removeUnusedDomains
          >>= \ret -> get >>= \g -> addLog "FinalState" [pretty g] >> return ret


      noteFormat "State" [pretty state]
      noteFormat "Start" [pretty sp]
      noteFormat "Final" [pretty sfin]

      return (sfin,state)

  where
  noteMsg tx s = do
      noteFormat ("@" <+> tx) []
      return s

  note tx f s = do
      noteFormat ("@" <+> tx <+> "Start") []

      newSp <- f s
      noteFormat ("@" <+> tx <+> "End") [pretty newSp]

      return newSp


tryRemoveConstraints :: Spec -> RR Spec
tryRemoveConstraints  sp@(Spec ds _ obj) = do
  runSpec (Spec ds [] obj) >>= \case
    Just r  -> do
      recordResult r
      return (Spec ds [] obj)
    Nothing -> return sp

removeUnusedDomains :: Spec -> RR Spec
removeUnusedDomains sp@(Spec ods es obj) = do
    let unusedNames = unusedDomains sp

    nds <- process (choices ods unusedNames)
    case nds of
        Just ds -> return (Spec ds es obj)
        Nothing -> return (Spec ods es obj)

    where
    choices :: Domains -> [Text] -> [Domains]
    choices ds ts =
        -- remove [] and reversing to get largest first
        -- meaning res would be [ [a], [b], [a,b],  ... ]
        let ways = reverse . tail . sortBy (comparing length) . subsequences $ ts
            res = fmap (\wy -> M.filterWithKey (\k _ -> k `notElem` wy) ds ) ways
        in res

    process :: [Domains]-> RR (Maybe Domains)
    process []     = return Nothing
    process (x:xs) = runSpec (Spec y es obj) >>= \case
        Just r  -> do
          recordResult r
          return $ Just y
        Nothing -> process xs

        where y = ensureADomain x


    ensureADomain :: Domains -> Domains
    ensureADomain ds | M.null ds = M.insert ("unused") (Findd DomainBool) ds
    ensureADomain ds = ds


removeConstraints :: Spec -> RR Spec
removeConstraints (Spec ds oes obj) = do
    let nubbed = nub2 oes
    nes <- process (choices nubbed)
    case nes of
        Just es -> return (Spec ds es obj)
        Nothing -> return (Spec ds nubbed obj)

    where

    choices :: [Expr] -> [[Expr]]
    choices ts =
        let ways = sortBy (comparing length) . subsequences $ ts
        in  ways

    process :: [[Expr]] -> RR (Maybe [Expr])
    process []     = return Nothing
    process (x:xs) = runSpec (Spec ds x obj) >>= \case
        Just r  -> do
          recordResult r
          return $ Just x
        Nothing -> process xs

    -- process ts = rrError . show . prettyBrackets . vcat $ map (prettyBrackets .  vcat . map pretty) ts

simplyConstraints :: Spec -> RR Spec
simplyConstraints sp@(Spec ds es obj) = do
    csToDo <- doConstraints es
    addLog "Got Constraints" []
    fin    <- process csToDo
    addLog "finished processing" []
    if fin == [] then
        runSpec (Spec ds [] obj) >>= \case
            Just r  -> do
              recordResult r
              return (Spec ds [] obj)
            Nothing -> return (Spec ds es obj)
    else
        return (Spec ds fin obj)

    where
    process :: [[Expr]] -> RR [Expr]

    -- cannot simply any futher
    process xs | any (== []) xs = return []

    process xs | all (singleElem) xs = do
        addLog "processsingleElem" []
        let fix = map head xs
        runSpec (Spec ds fix obj) >>= \case
          Just r -> do
            recordResult r
            return fix
          Nothing -> return []

    process esR = do
        addLog "process esR" []
        fix <- choose esR
        runSpec (Spec ds fix obj) >>= \case
                Nothing -> removeNext esR >>= process
                Just r  -> do
                    recordResult r
                    innerToDo <- doConstraints fix
                    inner     <- process innerToDo
                    if inner == [] then
                        return fix
                    else
                        return inner

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
