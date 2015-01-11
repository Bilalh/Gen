{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module TestGen.Reduce.Reduce where

import TestGen.Reduce.Data
import TestGen.Reduce.Runner
import TestGen.Reduce.Reduction
import TestGen.Reduce.UnusedDomains
import TestGen.Prelude

import qualified Data.Map as M



reduceMain :: SpecE -> RState -> IO SpecE
reduceMain sp rr  = do
    noteFormat "Starting with" [pretty sp]

    (sfin,state) <- (flip runStateT) rr $
        return sp
        >>= (note "removeUnusedDomains") removeUnusedDomains
        >>= (note "removeConstraints")   removeConstraints
        >>= (note "simplyConstraints")   simplyConstraints
        >>= (note "removeUnusedDomains") removeUnusedDomains


    noteFormat "State" [pretty state]
    noteFormat "Start" [pretty sp]
    noteFormat "Final" [pretty sfin]

    return sfin

    where
    note tx f s = do
        noteFormat ("@" <+> tx <+> "Start") []

        newSp <- f s
        noteFormat ("@" <+> tx <+> "End") [pretty newSp]

        return newSp

removeUnusedDomains :: SpecE -> RR SpecE
removeUnusedDomains sp@(SpecE ods es) = do
    let unusedNames = unusedDomains sp

    nds <- process (choices ods unusedNames)
    case nds of
        Just ds -> return (SpecE ds es)
        Nothing -> return (SpecE ods es)

    where
    choices :: Doms -> [Text] -> [Doms]
    choices ds ts =
        -- remove [] and reversing to get largest first
        -- meaning res would be [ [a], [b], [a,b],  ... ]
        let ways = reverse . tail . sortBy (comparing length) . subsequences $ ts
            res = fmap (\wy -> M.filterWithKey (\k _ -> k `notElem` wy) ds ) ways
        in res

    process :: [Doms]-> RR (Maybe Doms)
    process []     = return Nothing
    process (x:xs) = runSpec (SpecE y es) >>= \case
        True  -> return $ Just y
        False -> process xs

        where y = ensureADomain x


    ensureADomain :: Doms -> Doms
    ensureADomain ds | M.null ds = M.insert ("unused") (Find DBool) ds
    ensureADomain ds = ds


removeConstraints :: SpecE -> RR SpecE
removeConstraints (SpecE ds oes) = do
    let nubbed = nub2 oes
    nes <- process (choices nubbed)
    case nes of
        Just es -> return (SpecE ds es)
        Nothing -> return (SpecE ds nubbed)

    where

    choices :: [Expr] -> [[Expr]]
    choices ts =
        let ways = sortBy (comparing length) . subsequences $ ts
        in  ways

    process :: [[Expr]] -> RR (Maybe [Expr])
    process []     = return Nothing
    process (x:xs) = runSpec (SpecE ds x) >>= \case
        True  -> return $ Just x
        False -> process xs

    -- process ts = rrError . show . prettyBrackets . vcat $ map (prettyBrackets .  vcat . map pretty) ts

simplyConstraints :: SpecE -> RR SpecE
simplyConstraints sp@(SpecE ds es) = do
    csToDo <- doConstraints es
    fin <- process csToDo
    if fin == [] then
        runSpec (SpecE ds []) >>= \case
            True  -> return (SpecE ds [])
            False -> return (SpecE ds es)
    else
        return (SpecE ds fin)

    where
    process :: [[Expr]] -> RR [Expr]

    -- cannot simply any futher
    process xs | any (== []) xs = return []

    process xs | all (singleElem) xs = do
        let fix = map head xs
        res <- runSpec (SpecE ds fix)
        if res then do
            return fix
        else
            return []

    process esR = do
        fix <- choose esR
        res <- runSpec (SpecE ds fix)
        if res then do
            innerToDo <- doConstraints fix
            inner <- process innerToDo
            if inner == [] then
                return fix
            else
                return inner
        else
            removeNext esR >>= process

    -- Fix the next constraint
    choose :: [[Expr]] -> RR [Expr]
    choose esR = do
        return $ map pickFirst esR

        where
        pickFirst []    = error "pickfirst empty"
        pickFirst [x]   = x
        pickFirst (x:_) = x

    -- Keep the orginal exprs apart from the first
    doConstraints :: [Expr] -> RR [[Expr]]
    doConstraints [] = return [[]]
    doConstraints (x:xs) = do
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
