{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module TestGen.Reduce.Reduce where

import TestGen.Prelude
import qualified TestGen.Arbitrary.Arbitrary as A
import qualified TestGen.Arbitrary.Domain as A
import qualified TestGen.Arbitrary.Expr as A
import TestGen.Arbitrary.Data

import qualified Data.Map as M
import qualified Data.Text as T
import Data.List

import qualified Test.QuickCheck as QC

import TestGen.QCDebug(specE1)

import System.Random(randomIO)

class Reduce a where
    reduce :: a -> [a]
    single :: a -> [Expr]

instance Reduce Expr where
    reduce (EBinOp op) = single op ++ map EBinOp (reduce op) 
    reduce b = [b]

    single a = error "a"

instance Reduce BinOp where
    reduce (BOr a b) = map ( uncurry BOr ) $ 
        [ (a, etrue), (a,efalse), (etrue,b), (efalse, b) ]

    reduce b = [b]
    single b = [etrue,  efalse]

etrue  = ELit (EB True)
efalse = ELit (EB False)



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
    fin <- process (map simplyConstraint es)
    if fin == [] then
        return (SpecE ds es)
    else
        return (SpecE ds fin)
    
    where
    process :: [[Expr]] -> IO [Expr]
    process []  = error "process empty list"
    
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
            inner <- process (map simplyConstraint fix )
            if inner == [] then
                return fix
            else 
                return inner
        else 
            process (map tailR esR)
    
    singleElem [x] = True
    singleElem _   = False
    
    tailR :: [a] -> [a]    
    tailR []     = error "tailR empty list"
    tailR [x]    = [x]
    tailR (_:xs) = xs
        
    choose :: [[Expr]] -> IO [Expr]
    choose esR = do
        mapM rndExpr esR
        
    rndExpr :: [Expr] -> IO Expr
    rndExpr [] = error "empty"
    rndExpr xs = return (xs !! 0)

simplyConstraint :: Expr -> [Expr]
simplyConstraint e =  reduce e    
    
removeUnusedDomain :: SpecE -> IO SpecE    
removeUnusedDomain sp = undefined  

-- True means error still happens
runSpec :: SpecE -> IO Bool
runSpec sp = do
    stillErroed :: Bool <- randomIO 
    print $ (stillErroed, pretty sp)
    return stillErroed

-- wrap in a type to stop eval?
