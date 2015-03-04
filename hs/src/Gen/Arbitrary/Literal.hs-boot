module Gen.Arbitrary.Literal where

import Gen.AST.Imports
import Gen.Arbitrary.Data

boolLit :: GG Expr
intLit  :: GG Expr
matrixLitOf :: TType -> GG Expr

setLit   :: GG Expr
setLitOf ::  TType ->  GG Expr

msetLit   :: GG Expr
msetLitOf :: TType ->  GG Expr

funcLitOf  :: TType -> TType -> GG Expr
relLitOf   :: [TType] -> GG Expr
parLitOf   :: TType -> GG Expr
tupleLitOf :: [TType] -> GG Expr
