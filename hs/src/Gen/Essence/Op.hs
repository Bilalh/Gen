{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op (module X) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.EvalToInt
import Gen.Essence.Op.Internal.Generated as X
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Imports

instance (Generate a, ExpressionLike a, EvalToInt a, WrapConstant a) =>
    Generate (Op a) where
  give a = do
    withDepthDec $ frequency3 =<< getPossibilities a (allOps a)

  possible _ con = do
    bs <-  withDepthDec $ mapM check (allOps con)
    return $ or bs

    where
    check :: MonadState St m
          => ((GenerateConstraint -> m Bool), (Key, GenSt (Op a)))
          -> m Bool
    check (f,_) =  f con
