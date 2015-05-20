--This is an auto-generated file created by make update_ops_metadata
module Gen.Essence.Op.Internal.Generated (allOps) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Imports

import Gen.Essence.Op.Eq()
import Gen.Essence.Op.Geq()
import Gen.Essence.Op.Leq()
import Gen.Essence.Op.Mod()
import Gen.Essence.Op.Neq()
import Gen.Essence.Op.Or()
import Gen.Essence.Op.Union()


allOps :: forall m a
        . (Generate a, MonadState St m, Applicative m, ExpressionLike a)
       => GenerateConstraint
       -> [((GenerateConstraint -> m Bool ), (Key, GenSt (Op a)))]
allOps con = 
  [
 (possible (Proxy :: Proxy (OpEq a))               , (K_OpEq             , MkOpEq              <$> give con ))
  , (possible (Proxy :: Proxy (OpGeq a))              , (K_OpGeq            , MkOpGeq             <$> give con ))
  , (possible (Proxy :: Proxy (OpLeq a))              , (K_OpLeq            , MkOpLeq             <$> give con ))
  , (possible (Proxy :: Proxy (OpMod a))              , (K_OpMod            , MkOpMod             <$> give con ))
  , (possible (Proxy :: Proxy (OpNeq a))              , (K_OpNeq            , MkOpNeq             <$> give con ))
  , (possible (Proxy :: Proxy (OpOr a))               , (K_OpOr             , MkOpOr              <$> give con ))
  , (possible (Proxy :: Proxy (OpUnion a))            , (K_OpUnion          , MkOpUnion           <$> give con ))

  ]
