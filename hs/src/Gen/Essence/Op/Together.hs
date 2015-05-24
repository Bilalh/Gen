{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Together (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpTogether a) where
  give (GType TypeBool) = do
    inn <- dgive GNone
    OpTogether <$> give (GType $ TypeSet inn) <*> give (GType $ TypePartition inn)

  give t = giveUnmatched "Generate OpTogether" t

  possiblePure _ (Just TypeBool) d = d >= 1
  possiblePure _ Just{} _          = False
  possiblePure _ Nothing _         = False

  requires _ (Just TypeBool) = [RAll [K_TypeSet, K_TypePartition]]
  requires _ _               = []
