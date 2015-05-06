--This is an auto-generated file created by runhaskell scripts/add_ops_metadata.hs
module Gen.Essence.Op.Internal.Generated (allOps) where

import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Helpers.StandardImports

import Gen.Essence.Op.Eq()
import Gen.Essence.Op.Geq()
import Gen.Essence.Op.Leq()


allOps :: forall m a
        . (Generate a, MonadState St m, Applicative m)
       => GenerateConstraint
       -> [((Type -> m Bool ), (Key, GenSt (Op a)))]
allOps con = 
  [
 (possible (Proxy :: Proxy (OpEq a))               , (getId (Proxy :: Proxy (OpEq a))               , MkOpEq              <$> give con ))
  , (possible (Proxy :: Proxy (OpGeq a))              , (getId (Proxy :: Proxy (OpGeq a))              , MkOpGeq             <$> give con ))
  , (possible (Proxy :: Proxy (OpLeq a))              , (getId (Proxy :: Proxy (OpLeq a))              , MkOpLeq             <$> give con ))

  ]
