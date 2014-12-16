{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module TestGen.Reduce.Reduce where

import TestGen.Reduce.Data
import TestGen.Reduce.Runner
import TestGen.Reduce.Simpler

import TestGen.Prelude
import TestGen.Helpers.Runner(KindI(..))
import TestGen.QCDebug(specE1)

import qualified TestGen.Arbitrary.Arbitrary as A
import qualified TestGen.Arbitrary.Domain as A
import qualified TestGen.Arbitrary.Expr as A

import qualified Data.Map as M
import qualified Test.QuickCheck as QC

import Control.Arrow((&&&))
import System.Random(randomIO)


class Reduce a where
    reduce   :: a -> [a]    -- list of smaller exprs
    single   :: a -> [Expr] -- smallest literal e.g  [true, false] for  a /\ b
    subterms :: a -> [Expr] -- a /\ b  -->   [a, b]

    -- reduce a   = error "no default reduce"
    -- single a   = error "no default of single"
    -- subterms a = error "no default of subterms"

instance Reduce Expr where
    reduce (EBinOp op) = single op ++ subterms op ++ map EBinOp (reduce op) 
    
    reduce a   = [] -- no reductions possible   
    
    single a   = error "no single expr"
    subterms a = [] 
    

instance Reduce BinOp where
    reduce (BOr a b) = map ( uncurry BOr ) $  catMaybes
        [ (a, etrue) *| simpler etrue b , (a,efalse)  *| simpler efalse b
        , (etrue,b)  *| simpler etrue a , (efalse, b) *| simpler efalse a ]

    reduce b = []
    
    single (BOr _ _) = [etrue,  efalse]
    single (BEQ _ _) = [etrue,  efalse]
    
    single a = error . show . vcat   
        $ ["single missing case", pretty $ toEssence a, pretty $ groom a ]
    

    subterms (BOr a b) = [a,b] 
    subterms (BEQ a b) = [a,b] 
    

    subterms a = error . show . vcat   
        $ ["subterms missing case", pretty $ toEssence a, pretty $ groom a ]


reduceMain :: SpecE -> IO SpecE 
reduceMain s1 = do
    -- s1 <- removeConstraints sp
    s2 <- simplyConstraints s1
    
    sfin <- return s2 

    putStrLn "----"    
    putStrLn "Start"
    print . pretty $ s1
    putStrLn "----"    
    putStrLn "Final"
    print . pretty $ sfin
    putStrLn "----"    
    
    return sfin

    
simplyConstraints :: SpecE -> IO SpecE
simplyConstraints (SpecE ds es) = do
    fin <- process (doConstraints es)
    if fin == [] then do 
        runSpec (SpecE ds []) >>= \case
            True  -> return (SpecE ds [])
            False -> return (SpecE ds es)
            
        return (SpecE ds es)
    else
        return (SpecE ds fin)
    
    where
    process :: [[Expr]] -> IO [Expr]
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
            inner <- process (doConstraints fix )
            if inner == [] then
                return fix
            else 
                return inner
        else 
            removeNext esR >>= process
    

    
    tailR :: [a] -> [a]    
    tailR []     = error "tailR empty list"
    tailR [x]    = [x]
    tailR (_:xs) = xs
    
    -- Fix the next constraint
    choose :: [[Expr]] -> IO [Expr]
    choose esR = do
        return $ map pickFirst esR
        
        where 
        pickFirst []    = error "pickfirst empty"
        pickFirst [x]   = x
        pickFirst (x:_) = x 
        
    -- Keep the orginal exprs apart from the first
    doConstraints :: [Expr] -> [[Expr]]
    doConstraints []      = [[]]  
    doConstraints (x:xs)  = (reduce x) : map (\y -> y : reduce y) xs
           

    singleElem :: [a] -> Bool
    singleElem [x] = True
    singleElem _   = False
    
    removeNext :: [[a]] -> IO [[a]]
    removeNext []                     = error "removeNext empty"
    removeNext xs | all singleElem xs = return xs
    removeNext xs | any null xs       = error "removeNext sub empty"

    removeNext ([x]:xs)    = ([x]:)  <$> removeNext xs
    removeNext ((_:fs):xs) = return $ fs:xs
    removeNext (x:xs )     = (x:) <$> removeNext xs
    
removeUnusedDomain :: SpecE -> IO SpecE    
removeUnusedDomain sp = undefined  



_reduce e = 
    case  fromEssence e of 
        Left err -> error . show .  (pretty &&& pretty . groom)  $ err
        Right ee -> do
            let res = reduce ee
            mapM (print  . pretty . toEssence)  res
            return res
 
_e :: FromEssence a => E -> a
_e e =  case fromEssence e of 
        Left err -> error . show .  (pretty &&& pretty . groom) $ err
        Right ee -> ee


_tempRR = RState{oErrKind_         =Validate_
                ,oErrEprime_       = "/Users/bilalh/CS/break_conjure/fixed/46c3d2b43f4e/2014-12-10_02-01_1418176894/_errors/Validate_/ErrorUnknown_/1418178864_89/model000001.eprime"
                ,mostReduced_      = Nothing
                ,mostReducedFP_    = Nothing
                ,otherErrorsFound_ = []
                ,rgen_             = mkrGen 1}



aa :: RR Int 
aa = rndRangeM (1,5)


