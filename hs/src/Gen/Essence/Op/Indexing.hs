{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Indexing (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.Id
import Gen.Essence.Ints
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports
import Gen.Essence.EvalToInt


instance (Generate a, ExpressionLike a, GenInfo a, EvalToInt a)
    => Generate (OpIndexing a) where
  give (GType inn) = do
     k <- elemFreq3 =<< getWeights
          [ (K_TypeMatrix, K_TypeMatrix)
          , (K_TypeTuple, K_TypeTuple)
          ]
     d <- gets depth
     case k of
       K_TypeMatrix -> do
         logInfo2 $line ["K_TypeMatrix", nn "depth" d,  nn "inn" inn]
         -- TODO in bounds
         OpIndexing <$> give (GType $ TypeMatrix TypeInt inn) <*> give (GType TypeInt)
       K_TypeTuple -> do
         n <- choose3 (1,4)
         (IntRanged c ci) <- give (GIntRanged 1 n)
         logInfo2 $line ["K_TypeTuple"
                        , nn "depth" d
                        , nn "inn" inn
                        , nn "n" n
                        , nn "c" c]
         vs :: [Type] <- forM [1..n] $ \i -> do
                         if i == ci then
                             pure inn
                         else
                             dgive GNone
         logInfo2 $line $ "K_TypeTuple" : map pretty vs


         OpIndexing <$> give (GType $ TypeTuple vs)
                    <*> pure c

       _ -> $never

  give t = giveUnmatched "Generate OpIndexing" t

  possiblePure _ (Just ty) d =  fromIntegral d >= depthOf ty  + 1
  possiblePure _ Nothing _   = False

  requires _ (Just ty) = [RAll $ keyList ty, RAny [K_TypeMatrix, K_TypeMatrix]]
  requires _ _         = []
