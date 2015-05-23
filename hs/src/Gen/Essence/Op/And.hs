{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.And where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpAnd a) where
  give GNone = give (GType TypeBool)

  give (GType TypeBool) = OpAnd <$> give  (GType $ TypeMatrix TypeInt TypeBool)

  give t = giveUnmatched "Generate OpAnd" t

  possiblePure _ (Just TypeBool ) _ = True
  possiblePure _ Just{} _           = False
  possiblePure _ _ d                = d >= 1

  requires _ _ = [RAll [K_TypeBool]]
