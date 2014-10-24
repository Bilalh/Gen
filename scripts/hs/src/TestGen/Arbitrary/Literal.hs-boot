module TestGen.Arbitrary.Literal where

import AST.Imports
import TestGen.Arbitrary.Data
import Test.QuickCheck(Gen)


boolLit :: SpecState -> Gen Expr
intLit :: SpecState -> Gen Expr
matrixLitOf :: SpecState -> Type -> Gen Expr

setLit :: SpecState -> Gen Expr
setLitOf :: SpecState -> Type ->  Gen Expr

msetLit :: SpecState -> Gen Expr
msetLitOf :: SpecState -> Type ->  Gen Expr

funcLitOf :: SpecState -> Type -> Type -> Gen Expr
relLitOf :: SpecState -> [Type] -> Gen Expr
