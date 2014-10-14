module TestGen.Arbitrary.Literal where

import AST.Imports
import TestGen.Arbitrary.Data
import Test.QuickCheck(Gen)


boolLit :: SpecState -> Gen Expr
intLit :: SpecState -> Gen Expr
setLit :: SpecState -> Gen Expr
setLitOf :: SpecState -> Type ->  Gen Expr
