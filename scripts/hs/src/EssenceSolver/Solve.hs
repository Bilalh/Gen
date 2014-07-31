{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module EssenceSolver.Solve where

import Common.Helpers
import EssenceSolver.Data
import EssenceSolver.AllValues(allValues)
import EssenceSolver.Checker

-- import Language.E
import Language.E hiding (trace)
import Debug.Trace(trace)

import Bug
import Language.E.ValidateSolution
import Language.E.Evaluator(fullySimplify)

import Control.Monad(guard)
import Data.Map(Map)

import Control.Monad.State.Strict(State)

import qualified Data.Map as M
import qualified Text.PrettyPrint as P


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
    trace "   " $ mapM_ (guard . eSatisfied vs ) constraints
    return vs


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



ff :: Int -> String -> Int
ff  i  "a" = i * 2
ff  0   _  = 1
ff  i   _  = i - 1

-- Magically combine a list functions
compose :: [a -> a] -> (a -> a)
compose fs v = foldl (flip (.)) id fs $ v

xa :: String -> Int -> Bool
xa _ _ = True
