module Gen.Arbitrary.Op where

import Gen.AST.Imports
import Gen.Arbitrary.Data

type Bop = (Expr -> Expr -> Expr)
type Uop = (Expr -> Expr)
bop   :: Bop -> GG Expr
opOf  :: Uop -> TType ->  GG Expr
bopOf :: Bop -> TType -> GG Expr

equivExpr        ::  GG Expr
arithmeticExprOf ::  TType ->  GG Expr
relationExpr     ::  GG Expr

boolOpFor :: TType -> GG (Expr -> Expr -> Expr)
