{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module EssenceSolver.Solve where

import EssenceSolver.Data
import TestGen.Helpers

import Bug
import Language.E hiding (trace)
import Language.E.ValidateSolution
import Debug.Trace(trace)

import Control.Monad(guard)

import Data.Map(Map)
import qualified Data.Map as M

import qualified Text.PrettyPrint as P

-- Start with a Spec with finds and constraints
-- letting and given have allready been inlined

data State = State {
      tDoms :: Map Text E -- contains all values left in the domain
    , tConstraints :: [E] -- All constraints from the spec
    , tVars :: Map Text E --  current assigned values
} deriving Show

instance Pretty State where
    pretty State{..} = hang "State" 4 $
       P.braces . vcat . punctuate "," $ [
               "tDoms"        <+> "=" <+> (vcat . map pretty $ M.toList tDoms)
             , "tConstraints" <+> "=" <+> (vcat . map pretty $ tConstraints )
             , "tVars"        <+> "=" <+> (vcat . map pretty $ M.toList tVars )
       ]


solveSpec :: Spec -> Maybe Spec
solveSpec spec@(Spec _ stmt) =

    let
        es = statementAsList stmt
        tDoms = M.fromList . map (\(a,b) -> (getName a, b) ) .  pullFinds $ es
        tConstraints = pullConstraints $ es
        startState = State{tVars=M.fromList [],tDoms,tConstraints}

    in trace (show . pretty $ startState) $ Just spec



pullConstraints :: [E] -> [E]
pullConstraints = catMaybes . map f
    where f [xMatch| [xs] := topLevel.suchThat |] = Just xs
          f x = Nothing

allValues :: E -> [E]
allValues [xMatch| rs := domain.int.ranges |] =
    concatMap getIntVals rs

    where
        getIntVals [xMatch| [Prim (I j)] := range.single.value.literal |] =
            [mkInt j]
        getIntVals [xMatch| [Prim (I l), Prim(I u)] := range.fromTo.value.literal |] =
            map mkInt [l..u]
        getIntVals [xMatch| [Prim (I _)] := range.from.value.literal |]  =
            error "int unbounded"
        getIntVals _ = error "getIntVals"


-- Simple backtracking

type Choice a = [a]
choose :: [a] -> Choice a
choose xs = xs

solveConstraints =  do
    x <- choose [1,2,3 :: Integer]
    y <- choose [4,5,6]
    guard ( x + y > 4 )
    guard ( y == x +1 )
    return (x,y)

