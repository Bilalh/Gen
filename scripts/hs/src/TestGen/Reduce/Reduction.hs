{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module TestGen.Reduce.Reduction where

import TestGen.Reduce.Data
import TestGen.Reduce.Simpler

import TestGen.Prelude

import qualified TestGen.Arbitrary.Arbitrary as A
import qualified TestGen.Arbitrary.Domain as A
import qualified TestGen.Arbitrary.Expr as A

import qualified Data.Map as M
import qualified Test.QuickCheck as QC


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
    
    single (BOr _ _)  = [etrue,  efalse]
    single (BEQ _ _)  = [etrue,  efalse]
    single (BNEQ _ _) = [etrue,  efalse]
    
    single a = error . show . vcat   
        $ ["single missing case", pretty $ toEssence a, pretty $ groom a ]
    

    subterms (BOr a b)  = [a,b] 
    subterms (BEQ a b)  = [a,b] 
    subterms (BNEQ a b) = [a,b] 
    
    subterms a = error . show . vcat   
        $ ["subterms missing case", pretty $ toEssence a, pretty $ groom a ]

