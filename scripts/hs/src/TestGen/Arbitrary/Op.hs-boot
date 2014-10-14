module TestGen.Arbitrary.Op where

import AST.Imports
import TestGen.Arbitrary.Data
import Test.QuickCheck(Gen)

type Bop = (Expr -> Expr -> BinOp)
bop :: SpecState -> Bop ->  Gen Expr
opOf :: SS -> (Expr -> UniOp) -> Type ->  Gen Expr
bopOf :: SpecState -> Bop -> Type -> Gen Expr
bar :: SS -> Gen Expr
