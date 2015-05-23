{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Factorial where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpFactorial a) where
  give GNone = give (GType TypeInt)

  give ty@(GType TypeInt) = OpFactorial <$> give ty
  give t                  = giveUnmatched "Generate OpFactorial" t

  possiblePure _ (Just TypeInt ) _ = True
  possiblePure _ Just{} _          = False
  possiblePure _ _ d               = d >= 0

  requires _ _ = [RAll [K_TypeInt]]
