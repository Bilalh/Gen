{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TestGen.Reduce.Reduce where

import TestGen.Reduce.Data
import TestGen.Reduce.Runner
import TestGen.Reduce.Reduction
import TestGen.Reduce.UnusedDomains
import TestGen.Helpers.TypeOf

import TestGen.Prelude
import TestGen.Helpers.Runner(KindI(..), StatusI(..))
import TestGen.QCDebug(specE1)

import qualified TestGen.Arbitrary.Arbitrary as A
import qualified TestGen.Arbitrary.Domain as A
import qualified TestGen.Arbitrary.Expr as A

import qualified Data.Map as M
import qualified Test.QuickCheck as QC

import Control.Arrow((&&&))
import System.Random(randomIO)




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
    note tx f sp = do
        noteFormat ("@" <+> tx <+> "Start") []

        newSp <- f sp
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


_parts ::
        (ToEssence a1, FromEssence a) =>
        (a -> StateT EState Identity [a1]) -> E -> IO [a1]
_parts f e =
    case  fromEssence (e :: E) of
        Left er -> error . show .  (pretty &&& pretty . groom)  $ er
        Right ee -> do
            let spe   :: SpecE  = undefined
                seed            = 32
                state :: EState = EState{spec_=spe,sgen_=mkrGen seed,elogs_=LSEmpty}
                res             = runIdentity $ flip evalStateT state $ f ee
            mapM_ (print  . pretty . toEssence)  res
            return res

_partse :: ToEssence a =>
         (t -> StateT EState Identity [a]) -> t -> IO [a]
_partse f e = do
    let spe   :: SpecE  = undefined
        seed            = 32
        state :: EState = EState{spec_=spe,sgen_=mkrGen seed, elogs_=LSEmpty}
        res             = runIdentity $ flip evalStateT state $ f e
    mapM_ (print  . pretty . toEssence)  res
    return res


_e :: FromEssence a => E -> a
_e e =  case fromEssence e of
        Left er -> error . show .  (pretty &&& pretty . groom) $ er
        Right ee -> ee

_k :: IO SpecE
_k = do
    -- let fp = "/Users/bilalh/CS/break_conjure/misc/1419393045_122/spec.specE"
    -- let fp = "/Users/bilalh/CS/break_conjure/2014-12-19_04-19_1418962766/RefineCompact_/rrErrorUnknown_/1418964459_41/spec.specE"
    -- let fp = "/Users/bilalh/CS/break_conjure/2014-12-19_04-19_1418962766/RefineCompact_/rrErrorUnknown_/1418965624_49/spec.specE"
    -- let fp = "/Users/bilalh/CS/break_conjure/2014-12-19_04-19_1418962766/Savilerow_/ParseError_/1418964183_16/spec.specE"
    -- let fp = "/Users/bilalh/CS/break_conjure/misc/1418964183_16_r/spec.specE"
    let fp = "/Users/bilalh/CS/break_conjure/out/1420607973_828/spec.specE"

    spe <- readSpecE fp
    reduceMain spe
           def{oErrKind_   = Savilerow_
              ,oErrStatus_ = ParseError_
              ,oErrEprime_ = Nothing
              ,outputdir_  = "/Users/bilalh/CS/break_conjure/out"
              ,rgen_       = mkrGen 3
              }


-- _tempRR :: RState
-- _tempRR  = def{oErrKind_   = Validate_
--               ,oErrEprime_ = Just "/Users/bilalh/CS/break_conjure/fixed/46c3d2b43f4e/2014-12-10_02-01_1418176894/_rrErrors/Validate_/rrErrorUnknown_/1418178864_89/model000001.eprime"
--               ,outputdir_  = "/Users/bilalh/CS/break_conjure/fixed/46c3d2b43f4e/2014-12-10_02-01_1418176894/_rrErrors/Validate_/rrErrorUnknown_/1418178864_89/reduce/"
--               ,rgen_       = mkrGen 6
--               }
-- hellod
