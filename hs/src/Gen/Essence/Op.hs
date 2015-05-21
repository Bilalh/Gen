{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op (module X) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.EvalToInt
import Gen.Essence.Op.Internal.Generated as X
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Helpers.SizeOf
import Gen.Imports

instance (Generate a, ExpressionLike a, EvalToInt a, WrapConstant a)
    => Generate (Op a) where
  give a = do
      ops <- getPossibilities a (allOps a)
      withDepthDec $ frequency3 ops

  possible _ con@(GType ty) = do
    d <- gets depth
    case depthOf ty + 1 <= fromIntegral d of
      False -> return False
      True  -> do
        bs <- mapM (check) (allOps GNone)
        return $ or bs
    where
    check :: MonadState St m
          => ((GenerateConstraint -> m Bool), (Key, GenSt (Op a)))
          -> m Bool
    check (f,_) = f con

  possible _ _ = return False
