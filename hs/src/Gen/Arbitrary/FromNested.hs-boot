module Gen.Arbitrary.FromNested where
import Gen.Arbitrary.Prelude

nestedVarsOf :: Type -> GG (Maybe (GG Expr))
nestedVarsOf' :: Type -> GG (Maybe (GG Expr))
