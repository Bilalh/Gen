module TestGen.Arbitrary.Op where

import AST.Imports
import TestGen.Arbitrary.Data
import Test.QuickCheck(Gen)

type Bop = (Expr -> Expr -> BinOp)
type Uop = (Expr -> UniOp)
bop   :: Bop -> GG Expr
opOf  :: (Expr -> UniOp) -> Type ->  GG Expr
bopOf :: Bop -> Type -> GG Expr

equivExpr        ::  GG Expr
arithmeticExprOf ::  Type ->  GG Expr
relationExpr     ::  GG Expr

boolOpFor :: Type -> Gen (Expr -> Expr -> Expr)
