module TestGen.Arbitrary.FromNested where
import TestGen.Prelude

nestedVarsOf :: Type -> GG (Maybe (GG Expr))
nestedVarsOf' :: Type -> GG (Maybe (GG Expr))
