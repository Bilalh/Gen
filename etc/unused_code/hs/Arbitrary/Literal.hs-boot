module Gen.Arbitrary.Literal where

import Gen.AST.Imports
import Gen.Arbitrary.Data

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
aLiteral :: GG Expr