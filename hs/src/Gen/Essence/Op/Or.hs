{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Or where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpOr a) where
  give GNone            = give (GType TypeBool)
  give (GType TypeBool) = OpOr <$> give  (GType $ TypeMatrix TypeInt TypeBool)
  give t                = giveUnmatched "Generate OpOr" t

  possiblePure _ (Just TypeBool ) d = d >= 1
  possiblePure _ Just{} _           = False
  possiblePure _ _ d                = d >= 1

  requires _ _ = [RAll [K_TypeBool, K_TypeMatrix ]]
