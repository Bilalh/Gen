{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
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

import qualified Data.Map as M
import qualified Text.PrettyPrint as P

-- Start with a Spec with finds and constraints
-- letting and given have allready been inlined
solveSpec :: Spec -> Maybe Spec
solveSpec spec@(Spec _ stmt) =

    let
        es = statementAsList stmt
        tDoms = M.fromList . map (\(a,b) -> (getName a, b) ) .  pullFinds $ es
        tConstraints = pullConstraints $ es
        startState = State{tVars=M.fromList [],tDoms,tConstraints}

    in trace (show . pretty $ startState) $ Just spec

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

-- Simple backtracking

type Choice a = [a]
choose :: [a] -> Choice a
choose xs = xs

solveInts :: [(Integer, Integer)]
solveInts =  do
    x <- choose [1,2,3 :: Integer]
    y <- choose [4,5,6]
    guard ( x + y > 4 )
    guard ( y == x + 1 )
    return (x,y)

solveEInts :: [(E,E)]
solveEInts = do
    x <- choose $ [ [eMake| 1 |], [eMake| 2 |], [eMake| 3 |] ]
    y <- choose $ [ [eMake| 4 |], [eMake| 5 |], [eMake| 6 |] ]
    guard ( eguard [eMake| &x + &y > 4  |] )
    guard ( eguard [eMake| &y = &x + 1  |] )
    return (x, y)
