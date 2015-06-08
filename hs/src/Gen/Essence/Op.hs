{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op (module X) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.EvalToInt
import Gen.Essence.Op.Internal.Generated as X
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Imports

instance (Generate a, ExpressionLike a, EvalToInt a, GenInfo a) =>
    Generate (Op a) where
  give a = do
    (key,op) <- withDepthDec $ do
                 gets depth >>= \d -> logInfo2 $line [nn "depth" d, "Picking Op"]
                 elemFreq3 =<< pickOp a (allOps a)
    logInfo2 $line [nn "Picked" key]
    res <- withDepthDec $ do
             sanity "Generate Op"
             gets depth >>= \d -> logInfo2 $line [nn "depth" d, nn "key" key]
             op
    return res

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

pickOp :: arg -> [(arg -> GenSt Bool, (Key, v))]
              -> GenSt [(Int, (Key, v))]
pickOp con vs = do
  mapM doPossibilities vs

  where
  doPossibilities (f,(k,v)) =
   f con >>= \case
     False -> return (0,(k,v))
     True  -> do
       w <- weightingForKey k
       return (w,(k, v))
