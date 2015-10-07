module Gen.Arbitrary.Op where

import Gen.AST.Imports
import Gen.Arbitrary.Data

type Bop = (Expr -> Expr -> Expr)
type Uop = (Expr -> Expr)
bop   :: Bop -> GG Expr
opOf  :: Uop -> Type ->  GG Expr
bopOf :: Bop -> Type -> GG Expr

equivExpr        ::  GG Expr
arithmeticExprOf ::  Type ->  GG Expr
relationExpr     ::  GG Expr

boolOpFor :: Type -> GG (Expr -> Expr -> Expr)
