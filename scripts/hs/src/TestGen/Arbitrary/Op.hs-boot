module TestGen.Arbitrary.Op where

import AST.Imports
import TestGen.Arbitrary.Data
import Test.QuickCheck(Gen)

type Bop = (Expr -> Expr -> BinOp)
type Uop = (Expr -> UniOp)
bop :: SpecState -> Bop ->  Gen Expr
opOf :: SpecState -> (Expr -> UniOp) -> Type ->  Gen Expr
bopOf :: SpecState -> Bop -> Type -> Gen Expr
equivExpr :: SpecState -> Gen Expr
arithmeticExprOf :: SpecState -> Type ->  Gen Expr
relationExpr :: SpecState -> Gen Expr

boolOpFor :: Type -> Gen (Expr -> Expr -> Expr)
