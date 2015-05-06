{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Type where

import Gen.Essence.St
import Gen.Helpers.StandardImports
import Gen.Essence.Rnd

instance Generate TType where
  give GNone = do
    defs <- gets depth >>= \d ->
       if | d < 0     -> error $ "Generate TType invaild Depth: " ++ show d
          | d == 0    -> return [ ("TBool", pure TBool)
                                , ("TInt",  pure TInt)
                                ]
          | otherwise -> return [
                           ("TBool", pure TBool)
                         , ("TInt",  pure TInt)
                         , ("TSet",   liftM TSet   (withDepthDec (give GNone) ))
                         -- , ("TMatix", liftM TMatix (withDepthDec (give GNone) ))
                         -- , ("TMSet",  liftM TMSet  (withDepthDec (give GNone) ))
                         -- , ("TPar",   liftM TPar   (withDepthDec (give GNone) ))
                         ]

    parts <- getWeights defs
    frequency3 parts

  give GOnlyLiteralTypes = do
    defs <- gets depth >>= \d ->
       if | d <= 0     -> error $ "Generate TType(literal) invaild Depth: " ++ show d
          | otherwise -> return [
                           ("TSet",   liftM TSet   (withDepthDec (give GNone) ))
                         -- , ("TMatix", liftM TMatix (withDepthDec (give GNone) ))
                         -- , ("TMSet",  liftM TMSet  (withDepthDec (give GNone) ))
                         -- , ("TPar",   liftM TPar   (withDepthDec (give GNone) ))
                         ]

    parts <- getWeights defs
    frequency3 parts

  give t = giveUnmatched "Generate (TType)" t

  possiblePure _ _ _ = True
