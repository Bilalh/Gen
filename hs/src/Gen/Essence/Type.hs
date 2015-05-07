{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Type where

import Gen.Essence.St
import Gen.Imports
import Gen.Essence.Rnd

instance Generate Type where
  give GNone = do
    defs <- gets depth >>= \d ->
       if | d < 0     -> error $ "GenerateType invaild Depth: " ++ show d
          | d == 0    -> return [ ("TypeBool", pure TypeBool)
                                , ("TypeInt",  pure TypeInt)
                                ]
          | otherwise -> return [
                           ("TypeBool", pure TypeBool)
                         , ("TypeInt",  pure TypeInt)
                         , ("TypeSet",   liftM TypeSet   (withDepthDec (give GNone) ))
                         -- , ("TypeMatrix", liftM TypeMatrix (withDepthDec (give GNone) ))
                         -- , ("TypeMSet",  liftM TypeMSet  (withDepthDec (give GNone) ))
                         -- , ("TypePartition",   liftM TypePartition   (withDepthDec (give GNone) ))
                         ]

    parts <- getWeights defs
    frequency3 parts

  give GOnlyLiteralTypes = do
    defs <- gets depth >>= \d ->
       if | d <= 0     -> error $ "GenerateType(literal) invaild Depth: " ++ show d
          | otherwise -> return [
                           ("TypeSet",   liftM TypeSet   (withDepthDec (give GNone) ))
                         -- , ("TypeMatrix", liftM TypeMatrix (withDepthDec (give GNone) ))
                         -- , ("TypeMSet",  liftM TypeMSet  (withDepthDec (give GNone) ))
                         -- , ("TypePartition",   liftM TypePartition   (withDepthDec (give GNone) ))
                         ]

    parts <- getWeights defs
    frequency3 parts

  give t = giveUnmatched "Generate (Type)" t

  possiblePure _ _ _ = True
