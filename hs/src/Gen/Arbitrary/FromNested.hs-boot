module Gen.Arbitrary.FromNested where
import Gen.Prelude

nestedVarsOf :: Type -> GG (Maybe (GG Expr))
nestedVarsOf' :: Type -> GG (Maybe (GG Expr))
