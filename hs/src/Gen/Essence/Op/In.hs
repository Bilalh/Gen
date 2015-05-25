{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.In (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpIn a) where
  give (GType TypeBool) = do
     inn <- dgive GNone
     wrap <- elemFreq3 =<< getWeights
                          [ (K_TypeSet, TypeSet)
                          , (K_TypeMSet,TypeMSet)
                          ]
     OpIn <$> give (GType inn) <*> give (GType (wrap inn))


  give t = giveUnmatched "Generate OpIn" t

  possiblePure _ (Just TypeBool) d = d >=1
  possiblePure _ Just{} _          = False
  possiblePure _ Nothing _         = False

  requires _ (Just TypeBool) = [RAny [K_TypeSet,K_TypeMSet]]
  requires _ _               = []
