module TestGen.Arbitrary.FromNested where
import TestGen.Arbitrary.Helpers.Prelude

nestedVarsOf :: Type -> GG (Maybe (GG Expr))
nestedVarsOf' :: Type -> GG (Maybe (GG Expr))
