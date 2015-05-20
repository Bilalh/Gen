{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Not where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpNot a) where
  give GNone = give (GType TypeBool)

  give ty@(GType TypeBool) = do
    OpNot <$> give ty

  give t = giveUnmatched "Generate OpNot" t

  possiblePure _ (Just ty)  _ | ty /= TypeBool = False
  possiblePure _ _ d = d >= 1

  requires _ _ = [RAll [K_TypeBool]]
