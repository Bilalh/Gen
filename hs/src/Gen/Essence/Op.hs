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
    d <- gets depth
    if d <= 0 then
        return False
    else do
      logDepthCon $line con
      bs <- withDepthDec $ mapM check (allOps con)
      logInfo2 $line ("ops:" :  [pretty  k | (b,k) <- bs , b ]  )
      return $ or $ (map fst) bs

    where
    check :: MonadState St m
          => ((GenerateConstraint -> m Bool), (Key, GenSt (Op a)))
          -> m (Bool, Key)
    check (f,(k,_)) = do
      res <- f con
      return (res,k)
