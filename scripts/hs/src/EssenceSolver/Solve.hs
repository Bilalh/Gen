{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module EssenceSolver.Solve where

import Common.Helpers
import EssenceSolver.Data
import EssenceSolver.AllValues(allValues)
import EssenceSolver.Checker

import Language.E
-- import Language.E hiding (trace)
-- import Debug.Trace(trace)

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

firstSolution :: Spec -> Maybe [(Ref, E)]
firstSolution (Spec _ stmt) =

    let
        es = statementAsList stmt
        tFinds = map (\(a,b) -> (getName a, b) ) .  pullFinds $ es
        tDoms  = map (\(a,b) -> (a, allValues b)) tFinds
        tConstraints = pullConstraints $ es

        sol = dfsSolve tDoms tConstraints
    in fmap (map (\(n,v) -> (mkName n, v)) ) sol


dfsSolve :: [DomVals] ->  [Constraint] -> Maybe Env
dfsSolve a b = solve a b []

    where
    solve :: [DomVals] ->  [Constraint] -> Env -> Maybe Env
    solve ds@(_:_) [] [] = let vs =  map f ds in
            case all isJust vs of
                True -> Just $ catMaybes vs
                False -> Nothing

        where f (_, [])    = Nothing
              f (t, (e:_)) = Just (t, e)

    solve [] _ env = Just env
    solve ( (dname, dvals) : drest) cs  env =
        case dvals of
            []     -> Nothing
            (x:xs) -> let newEnv = updateEnv env (dname,x) in
                case violates cs newEnv of
                    True  -> tracePretty  [ "violated after"  <+> pretty dname,  prettyEnv newEnv]
                        solve ( (dname, xs) : drest ) cs env
                    False -> tracePretty [ "Assigned" <+> pretty dname,  prettyEnv newEnv  ] $
                        case solve ( drest ) cs newEnv of
                            Just jenv -> Just jenv
                            Nothing -> solve ( (dname, xs) : drest ) cs env


updateEnv :: Env -> DomVal  -> Env
updateEnv env val = val : env


firstSolutionAP :: Spec -> Maybe [(Ref, E)]
firstSolutionAP = listToMaybe . allSolutions

allSolutions :: Spec -> [[(Ref, E)]]
allSolutions (Spec _ stmt) =

    let
        es = statementAsList stmt
        tFinds = map (\(a,b) -> (a, b) ) .  pullFinds $ es
        tDoms  = map (\(a,b) -> (a, allValues b)) tFinds
        tConstraints = pullConstraints $ es

        sols = allPossibilitiesSolve tDoms tConstraints
    in sols

allPossibilitiesSolve :: [(Ref, [E])] -> [E] -> [[(Ref, E)]]
allPossibilitiesSolve domValues constraints = do
    vs <- allCombinations domValues
    trace "   " $ mapM_ (guard . eSatisfied vs ) constraints
    return vs


