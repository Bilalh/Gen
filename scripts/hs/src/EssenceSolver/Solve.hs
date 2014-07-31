{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module EssenceSolver.Solve where

import Common.Helpers
import EssenceSolver.Data
import EssenceSolver.AllValues(allValues)

import Bug
import Language.E hiding (trace)
import Language.E.ValidateSolution

import Control.Monad(guard)
import Debug.Trace(trace)
import Data.Map(Map)

import Control.Monad.State.Strict(State)

import qualified Data.Map as M
import qualified Text.PrettyPrint as P

type Ref = E

-- Start with a Spec with finds and constraints
-- letting and given have allready been inlined
-- solveSpec :: Spec -> Maybe Spec
solveSpec (Spec _ stmt) =

    let
        es = statementAsList stmt
        -- tFinds = map (\(a,b) -> (getName a, b) ) .  pullFinds $ es
        tFinds = map (\(a,b) -> (a, b) ) .  pullFinds $ es
        tDoms  = map (\(a,b) -> (a, allValues b)) tFinds
        tConstraints = pullConstraints $ es

        sols = backtrackSolve tDoms tConstraints
    in firstSolution sols

backtrackSolve :: [(Ref, [E])] -> [E] -> [[(Ref, E)]]
backtrackSolve domValues constraints = do
    vs <- allCombinations domValues
    mapM_ (guard . eSatisfied vs ) constraints
    return vs

eSatisfied :: [(Ref, E)] -> E -> Bool
eSatisfied vs e = subAndCheck

    where
    subAndCheck =
        let
            (subbedE, _logs) = runCompESingle "subVals"  (subVals vs e)
        in
            case subbedE of
                Left  x -> error . show $ vcat ["subAndCheck", x]
                Right ne ->
                    let sat =  eguard ne  in
                    trace (show $ vcat [pretty ne, pretty sat]) $  eguard ne


subVals  :: MonadConjure m => [(Ref, E)] -> E ->  m E
subVals lettings expr =
    let
        lettingsMap = M.fromList lettings

        f x | Just y <- M.lookup x lettingsMap = transform f y
        f x = x
    in
        return $ transform f expr


eguard :: E -> Bool
eguard e =
    let (mresult, _logs) = runCompESingle "eguard" helper
    in case mresult of
        Right b -> b
        Left d  -> error . show .  vcat $ ["eguard", d]

    where
        helper = do
            res <- toBool e
            return $ case res of
                Right (b,_) -> b
                Left _  -> False


firstSolution :: [a] -> Maybe a
firstSolution (x:_) = Just x
firstSolution []    = Nothing

aa :: State String String
aa =  do
    return ""

greeter :: (MonadState String m, Monad m) => m [String]
greeter = do
    name <- get
    put "tintin"
    return ["hello, " ++ name ++ "!"]

-- Simple backtracking
backtrackSolve1 :: [(E,E)]
backtrackSolve1  = do
    [x,y] <- mapM allValues [ [dMake| int(1..3) |],  [dMake| int(4..6) |] ]
    guard ( eguard [eMake| &x + &y > 4  |] )
    guard ( eguard [eMake| &y = &x + 1  |] )
    return (x, y)

solveEInts2 :: [(E,E)]
solveEInts2 = do
    x <- choose $ allValues [dMake| int(1..3) |]
    y <- choose $ allValues [dMake| int(4..6) |]
    guard ( eguard [eMake| &x + &y > 4  |] )
    guard ( eguard [eMake| &y = &x + 1  |] )
    return (x, y)

solveEInts :: [(E,E)]
solveEInts = do
    x <- choose $ [ [eMake| 1 |], [eMake| 2 |], [eMake| 3 |] ]
    y <- choose $ [ [eMake| 4 |], [eMake| 5 |], [eMake| 6 |] ]
    guard ( eguard [eMake| &x + &y > 4  |] )
    guard ( eguard [eMake| &y = &x + 1  |] )
    return (x, y)

solveInts :: [(Integer, Integer)]
solveInts =  do
    x <- choose [1,2,3 :: Integer]
    y <- choose [4,5,6]
    guard ( x + y > 4 )
    guard ( y == x + 1 )
    return (x,y)

type Choice a = [a]
choose :: [a] -> Choice a
choose xs = xs
