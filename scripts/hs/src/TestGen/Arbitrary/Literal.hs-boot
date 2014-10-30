module TestGen.Arbitrary.Literal where

import AST.Imports
import TestGen.Arbitrary.Data
import Test.QuickCheck(Gen)

boolLit :: GG Expr
intLit  :: GG Expr
matrixLitOf :: Type -> GG Expr

setLit   :: GG Expr
setLitOf ::  Type ->  GG Expr

msetLit   :: GG Expr
msetLitOf :: Type ->  GG Expr

funcLitOf  :: Type -> Type -> GG Expr
relLitOf   :: [Type] -> GG Expr
parLitOf   :: Type -> GG Expr
tupleLitOf :: [Type] -> GG Expr
