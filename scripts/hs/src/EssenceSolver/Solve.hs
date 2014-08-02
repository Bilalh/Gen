{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
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

type Env        = [(Text, E)]
type Constraint = E
type DomVals    = (Text, [E])
type DomVal     = (Text, E)

tracePretty :: [Doc] -> a -> a
tracePretty ds =  trace (show $ vcat ds)
traceHang :: Doc -> [Doc] -> a -> a
traceHang msg ds = trace ( show $ hang msg 4 (vcat ds) )

prettyEnv :: Env -> Doc
prettyEnv [] = hang "" 4 $ "env: []"
prettyEnv vs = hang "" 4 $  "env:" <+> (vcat . map pretty $ vs)


-- Start with a Spec with finds and constraints
-- letting and given have allready been inlined

allSolutions :: Spec -> [[(Ref, E)]]
allSolutions (Spec _ stmt) =

    let
        es = statementAsList stmt
        -- tFinds = map (\(a,b) -> (getName a, b) ) .  pullFinds $ es
        tFinds = map (\(a,b) -> (a, b) ) .  pullFinds $ es
        tDoms  = map (\(a,b) -> (a, allValues b)) tFinds
        tConstraints = pullConstraints $ es

        sols = allPossibilitiesSolve tDoms tConstraints
    in sols

firstSolutionAP :: Spec -> Maybe [(Ref, E)]
firstSolutionAP sp = onlyFirstSolution $ allSolutions sp

    where
    onlyFirstSolution :: [a] -> Maybe a
    onlyFirstSolution (x:_) = Just x
    onlyFirstSolution []    = Nothing

allPossibilitiesSolve :: [(Ref, [E])] -> [E] -> [[(Ref, E)]]
allPossibilitiesSolve domValues constraints = do
    vs <- allCombinations domValues
    trace "   " $ mapM_ (guard . eSatisfied vs ) constraints
    return vs


firstSolution :: Spec -> Maybe [(Ref, E)]
firstSolution (Spec _ stmt) =

    let
        es = statementAsList stmt
        tFinds = map (\(a,b) -> (getName a, b) ) .  pullFinds $ es
        tDoms  = map (\(a,b) -> (a, allValues b)) tFinds
        tConstraints = pullConstraints $ es

        sol = dfsSolve tDoms tConstraints []
    in fmap (map (\(n,v) -> (mkName n, v)) ) sol

dfsSolve :: [DomVals] ->  [Constraint] -> Env -> Maybe Env
dfsSolve ds@(_:_) [] [] =  error "ToDO"

dfsSolve [] _ env = Just env
dfsSolve ( (dname, dvals) : drest) cs  env =
    case dvals of
        []     -> Nothing
        (x:xs) -> let newEnv = updateEnv env (dname,x) in
                    case violates cs newEnv of
                        True  -> tracePretty  [ "violated after"  <+> pretty dname,  prettyEnv newEnv]
                            dfsSolve ( (dname, xs) : drest ) cs env
                        False -> tracePretty [ "Assigned" <+> pretty dname,  prettyEnv newEnv  ] $
                            case dfsSolve ( drest ) cs newEnv of
                                Just jenv -> Just jenv
                                Nothing -> dfsSolve ( (dname, xs) : drest ) cs env

updateEnv :: Env -> DomVal  -> Env
updateEnv env val = val : env

-- Returns True if any constraint is not satisfied
violates  :: [Constraint] -> Env -> Bool
violates cs env =
    let (mresult, _logs) = runCompESingle "violates" helper
    in case mresult of
        Right b    -> tracePretty ["violates result" <+> pretty b] b
        Left d     -> error . show .  vcat $ ["violates", d, pretty _logs]

    where
    helper :: MonadConjure m => m Bool
    helper = do
        mapM_ (\(n,e) -> addReference n e )  env

        violated :: Bool <- or <$> mapM eViolates cs
        return violated

    eViolates :: MonadConjure m =>  Constraint -> m Bool
    eViolates e = do
        simplifed <- fullySimplifyE e
        res <- toBool simplifed
        return $ case res of
            Right (b,x) -> traceHang ("EV" <+> pretty (not b)) [vcat (map pretty x), prettyEnv env, pretty e]
                           $ not b
            Left m      -> tracePretty ["eViolates constraint" <+> pretty m, prettyEnv env] False


-- ideas

aa :: State String String
aa =  do
    return ""

greeter :: (MonadState String m, Monad m) => m [String]
greeter = do
    name <- get
    put "tintin"
    return ["hello, " ++ name ++ "!"]

solveEInts3 :: [(E,E)]
solveEInts3  = do
    [x,y] <- mapM allValues [ [dMake| int(1..3) |],  [dMake| int(4..6) |] ]
    guard ( eguard [eMake| &x + &y > 4  |] )
    guard ( eguard [eMake| &y = &x + 1  |] )
    return (x, y)

solveEInts2 :: [(E,E)]
solveEInts2 = do
    x <- allValues [dMake| int(1..3) |]
    y <- allValues [dMake| int(4..6) |]
    guard ( eguard [eMake| &x + &y > 4  |] )
    guard ( eguard [eMake| &y = &x + 1  |] )
    return (x, y)

solveInts :: [(Integer, Integer)]
solveInts =  do
    x <-  [1,2,3 :: Integer]
    y <-  [4,5,6]
    guard ( x + y > 4 )
    guard ( y == x + 1 )
    return (x,y)

ff :: Int -> String -> Int
ff  i  "a" = i * 2
ff  0   _  = 1
ff  i   _  = i - 1

-- Magically combine a list functions
compose :: [a -> a] -> (a -> a)
compose fs v = foldl (flip (.)) id fs $ v

xa :: String -> Int -> Bool
xa _ _ = True
