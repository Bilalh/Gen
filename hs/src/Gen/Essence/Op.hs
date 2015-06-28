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
    logDepthCon $line a
    sanityn 1 "Trying Op with depth <1"
    (key,op) <- withDepthDec $ do
                 gets depth >>= \d -> logInfo2 $line ["Picking the op", nn "depth" d]
                 elemFreq3 =<< pickOp a (allOps a)
    logInfo2 $line [nn "Picked" key]
    res <- withDepthDec $ do
             sanity "Generate Op"
             gets depth >>= \d -> logInfo2 $line [nn "depth" d, nn "key" key]
             withKey key op
    return res

  possible _ con = do
    d <- gets depth
    if d <= 0 then
        return False
    else do
      logDepthCon $line con
      bs <- withDepthDec $ mapM check (allOps con)
      logWarn2 $line ("ops:" :  [pretty  k | (b,k) <- bs , b ]  )
      return $ or $ (map fst) bs

    where
    check :: MonadState St m
          => ((GenerateConstraint -> m Bool), (Key, GenSt (Op a)))
          -> m (Bool, Key)
    check (f,(k,_)) = do
      res <- f con
      w <- weightingForKey k
      return (res && w > 0,k)

pickOp :: Pretty arg
       => arg -> [(arg -> GenSt Bool, (Key, v))]
       -> GenSt [(Int, (Key, v))]
pickOp con vs = do
  res <- mapM doPossibilities vs
  logWarn2 $line $ nn "con" con : map pretty [ (k,v) | (v,(k,_)) <- res]
  when (null [ v | (v,(_,_)) <- res, v >0]) $
    lineError $line $ "pickOp all weights using" <+> pretty con :
              map pretty [ (k,v) | (v,(k,_)) <- res]

  return res

  where
  doPossibilities (f,(k,v)) =
   (withKey k $ f con) >>= \case
     False -> return (0,(k,v))
     True  -> do
       w <- withKey k $ weightingForKey k
       return (w,(k, v))
