--This is an auto-generated file created by make update_ops_metadata
module Gen.Essence.Op.Internal.Generated (allOps) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Imports

import Gen.Essence.Op.And()
import Gen.Essence.Op.Div()
import Gen.Essence.Op.Eq()
import Gen.Essence.Op.Factorial()
import Gen.Essence.Op.Geq()
import Gen.Essence.Op.Gt()
import Gen.Essence.Op.Intersect()
import Gen.Essence.Op.Leq()
import Gen.Essence.Op.Lt()
import Gen.Essence.Op.Mod()
import Gen.Essence.Op.Neq()
import Gen.Essence.Op.Or()
import Gen.Essence.Op.Pow()
import Gen.Essence.Op.Subset()
import Gen.Essence.Op.SubsetEq()
import Gen.Essence.Op.Supset()
import Gen.Essence.Op.SupsetEq()
import Gen.Essence.Op.ToInt()
import Gen.Essence.Op.ToMSet()
import Gen.Essence.Op.ToSet()
import Gen.Essence.Op.TwoBars()
import Gen.Essence.Op.Union()


allOps :: forall m a
        . (Generate a, MonadState St m, Applicative m, ExpressionLike a)
       => GenerateConstraint
       -> [((GenerateConstraint -> m Bool ), (Key, GenSt (Op a)))]
allOps con = 
  [
 (possible (Proxy :: Proxy (OpAnd a))              , (K_OpAnd            , MkOpAnd             <$> give con ))
  , (possible (Proxy :: Proxy (OpDiv a))              , (K_OpDiv            , MkOpDiv             <$> give con ))
  , (possible (Proxy :: Proxy (OpEq a))               , (K_OpEq             , MkOpEq              <$> give con ))
  , (possible (Proxy :: Proxy (OpFactorial a))        , (K_OpFactorial      , MkOpFactorial       <$> give con ))
  , (possible (Proxy :: Proxy (OpGeq a))              , (K_OpGeq            , MkOpGeq             <$> give con ))
  , (possible (Proxy :: Proxy (OpGt a))               , (K_OpGt             , MkOpGt              <$> give con ))
  , (possible (Proxy :: Proxy (OpIntersect a))        , (K_OpIntersect      , MkOpIntersect       <$> give con ))
  , (possible (Proxy :: Proxy (OpLeq a))              , (K_OpLeq            , MkOpLeq             <$> give con ))
  , (possible (Proxy :: Proxy (OpLt a))               , (K_OpLt             , MkOpLt              <$> give con ))
  , (possible (Proxy :: Proxy (OpMod a))              , (K_OpMod            , MkOpMod             <$> give con ))
  , (possible (Proxy :: Proxy (OpNeq a))              , (K_OpNeq            , MkOpNeq             <$> give con ))
  , (possible (Proxy :: Proxy (OpOr a))               , (K_OpOr             , MkOpOr              <$> give con ))
  , (possible (Proxy :: Proxy (OpPow a))              , (K_OpPow            , MkOpPow             <$> give con ))
  , (possible (Proxy :: Proxy (OpSubset a))           , (K_OpSubset         , MkOpSubset          <$> give con ))
  , (possible (Proxy :: Proxy (OpSubsetEq a))         , (K_OpSubsetEq       , MkOpSubsetEq        <$> give con ))
  , (possible (Proxy :: Proxy (OpSupset a))           , (K_OpSupset         , MkOpSupset          <$> give con ))
  , (possible (Proxy :: Proxy (OpSupsetEq a))         , (K_OpSupsetEq       , MkOpSupsetEq        <$> give con ))
  , (possible (Proxy :: Proxy (OpToInt a))            , (K_OpToInt          , MkOpToInt           <$> give con ))
  , (possible (Proxy :: Proxy (OpToMSet a))           , (K_OpToMSet         , MkOpToMSet          <$> give con ))
  , (possible (Proxy :: Proxy (OpToSet a))            , (K_OpToSet          , MkOpToSet           <$> give con ))
  , (possible (Proxy :: Proxy (OpTwoBars a))          , (K_OpTwoBars        , MkOpTwoBars         <$> give con ))
  , (possible (Proxy :: Proxy (OpUnion a))            , (K_OpUnion          , MkOpUnion           <$> give con ))

  ]
