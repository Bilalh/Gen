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
