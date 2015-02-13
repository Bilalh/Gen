{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module EssenceSolver.Solve where

import Common.Helpers
import EssenceSolver.Data
import EssenceSolver.AllValues(allValues)
import EssenceSolver.Checker

import Language.E

import Bug
import Language.E.ValidateSolution
import Language.E.Evaluator(fullySimplify)

import Control.Monad(guard)
import Data.Map(Map)
import Data.Set(Set)

import Control.Monad.State.Strict(State)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.PrettyPrint as P


-- Start with a Spec with finds and constraints
-- letting and given have allready been inlined

firstSolution :: Spec -> Maybe [(Ref, E)]
firstSolution (Spec _ stmt) =

    let
        es = statementAsList stmt
        tFinds = map (\(a,b) -> (getName a, b) ) .  pullFinds $ es
        tDoms  = map (\(a,b) -> (a, allValues b)) tFinds
        tNames = S.fromList $ (map fst) tFinds
        tConstraints =  pullConstraints $ es
        tOrdered = [ (S.fromList $  allVarsUsed tNames c, c)
                   | c <- tConstraints
                   ]
        tTrie = mkTrie ( map fst tFinds) tOrdered

    -- in  error . show $ vcat [ vcat $ map pretty  tOrdered
    --                             , pretty  tTrie]
        sol = dfsSolve tDoms tTrie
    in
        -- error $ show $ map (length . snd) tDoms
        fmap (map (\(n,v) -> (mkName n, v)) ) sol


dfsSolve :: [DomVals] ->  Trie Constraint -> Maybe Env
dfsSolve a b = solve a b []

    where
    solve :: [DomVals] -> Trie Constraint -> Env -> Maybe Env
    solve [] _ []  = Nothing   -- No Variables
    solve [] _ env = Just env  -- Assigned all variables successfully

    -- Variables without any constraints
    -- Assign the first value in its domain
    solve ds@(_:_) TNone env = let vs =  map f ds in
            case all isJust vs of
                True  -> Just $ catMaybes vs ++ env
                False -> Nothing

        where f (_, [])    = Nothing
              f (t, (e:_)) = Just (t, e)

    -- dfs search
    solve ( (dname, dvals) : drest) trie@(TSome _ cs trest) env =
        case dvals of
        []     -> Nothing  -- no values left in the domain

        (x:xs) -> let newEnv = updateEnv env (dname,x) in
            case violates cs newEnv of
                True -> tracePretty  ["violated after"<+> pretty dname, prettyEnv newEnv]
                    solve ( (dname, xs) : drest ) trie env

                False -> tracePretty ["Assigned" <+> pretty dname,  prettyEnv newEnv  ] $
                    case solve ( drest ) trest newEnv of
                        Just jenv -> Just jenv
                        Nothing -> solve ( (dname, xs) : drest ) trie env


updateEnv :: Env -> DomVal  -> Env
updateEnv env val = val : env

allVarsUsed ::  Set Text -> E ->  [Text]
allVarsUsed varNames statement  =
    [ nm
    | [xMatch| [Prim (S nm)] := reference |] <- universe statement
    , nm `S.member` varNames
    ]


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


