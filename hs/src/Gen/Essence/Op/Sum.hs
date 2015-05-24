{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Sum where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpSum a) where
  give GNone = give (GType TypeInt)

  give (GType TypeInt) = OpSum <$> give  (GType $ TypeMatrix TypeInt TypeInt)

  give t = giveUnmatched "Generate OpSum" t

  possiblePure _ (Just TypeInt ) d = d >=1
  possiblePure _ Just{} _          = False
  possiblePure _ _ d               = d >= 1

  requires _ _ = [RAll [K_TypeInt]]
