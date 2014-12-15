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
    return s2

    
simplyConstraints :: SpecE -> IO SpecE
simplyConstraints (SpecE ds es) = do
    fin <- process (map simplyConstraint es)
    return (SpecE ds fin)
    
    where
    process :: [[Expr]] -> IO [Expr]
    process []  = error "process empty list"
    
    process xs | all (singleElem) xs = return $ map head xs
        where
        singleElem [x] = True
        singleElem _   = False
        
    process esR = do
        fix <- choose esR
        res <- runSpec (SpecE ds fix)
        if res then do
            process (map simplyConstraint fix )
            
        else 
            process (map tailR esR)
    
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
    print $ pretty sp
    return True 

-- wrap in a type to stop eval?
