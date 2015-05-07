--This is an auto-generated file created by make update_ops_metadata
module Gen.Essence.Op.Internal.Generated (allOps) where

import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Imports

import Gen.Essence.Op.Eq()
import Gen.Essence.Op.Geq()
import Gen.Essence.Op.Leq()
import Gen.Essence.Op.Union()


allOps :: forall m a
        . (Generate a, MonadState St m, Applicative m)
       => GenerateConstraint
       -> [((Type -> m Bool ), (Key, GenSt (Op a)))]
allOps con = 
  [
 (possible (Proxy :: Proxy (OpEq a))               , (getId (Proxy :: Proxy (OpEq a))               , MkOpEq              <$> give con ))
  , (possible (Proxy :: Proxy (OpGeq a))              , (getId (Proxy :: Proxy (OpGeq a))              , MkOpGeq             <$> give con ))
  , (possible (Proxy :: Proxy (OpLeq a))              , (getId (Proxy :: Proxy (OpLeq a))              , MkOpLeq             <$> give con ))
  , (possible (Proxy :: Proxy (OpUnion a))            , (getId (Proxy :: Proxy (OpUnion a))            , MkOpUnion           <$> give con ))

  ]
