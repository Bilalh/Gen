{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Hist (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports

instance (Generate a, ExpressionLike a) => Generate (OpHist a) where
  give GNone = do
    ty <- dgive GNone
    give (GType (TypeMatrix TypeInt (TypeTuple [ty,TypeInt])))

  give (GType (TypeMatrix TypeInt (TypeTuple [inn,TypeInt])) ) = do
      wrap <- elemFreq3 =<< getWeights [ (K_TypeMatrix,TypeMatrix TypeInt)
                                       , (K_TypeSet,   TypeSet)
                                       , (K_TypeMSet,  TypeMSet)
                                       ]
      OpHist <$> give (GType $ wrap inn)

  give t = giveUnmatched "Generate OpHist" t

  possiblePure _ (Just (TypeMatrix TypeInt (TypeTuple [inn,TypeInt])))  d =
      (fromIntegral d) +1 >= depthOf inn

  possiblePure _ Just{} _ = False
  possiblePure _ _ d      = d>=1

  requires _ _ = [RAny $ [K_TypeSet, K_TypeMSet, K_TypeMatrix]]
