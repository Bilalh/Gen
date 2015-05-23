{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Product where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpProduct a) where
  give GNone = give (GType TypeInt)

  give (GType TypeInt) = OpProduct <$> give  (GType $ TypeMatrix TypeInt TypeInt)

  give t = giveUnmatched "Generate OpProduct" t

  possiblePure _ (Just TypeInt ) _ = True
  possiblePure _ Just{} _          = False
  possiblePure _ _ d               = d >= 1

  requires _ _ = [RAll [K_TypeInt]]
