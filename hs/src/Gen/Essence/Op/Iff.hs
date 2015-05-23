{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Iff where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpIff a) where
  give GNone = give (GType TypeBool)

  give ty@(GType TypeBool) = do
    OpIff <$> give ty <*> give ty

  give t = giveUnmatched "Generate OpIff" t

  possiblePure _ (Just TypeBool ) _ = True
  possiblePure _ Just{} _           = False
  possiblePure _ _ d                = d >= 0

  requires _ _ = [RAll [K_TypeBool]]
