{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Imply where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpImply a) where
  give GNone = give (GType TypeBool)

  give ty@(GType TypeBool) = do
    OpImply <$> give ty <*> give ty

  give t = giveUnmatched "Generate OpImply" t

  possiblePure _ (Just ty)  _ | ty /= TypeBool = False
  possiblePure _ _ d = d >= 0

  requires _ _ = [RAll [K_TypeBool]]
