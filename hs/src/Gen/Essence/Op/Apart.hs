{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Apart (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports

instance (Generate a, ExpressionLike a) => Generate (OpApart a) where
  give GNone = give (GType TypeBool)

  give (GType TypeBool) = do
    ty <- dgive GNone
    OpApart <$> give (GType $ TypeSet ty) <*> give (GType $ TypePartition ty)

  give t = giveUnmatched "Generate OpApart" t

  possiblePure _ (Just TypeBool ) _ = True
  possiblePure _ Just{} _           = False
  possiblePure _ _ d                = d >= 1

  requires _ _ = [RAll $ [K_TypeSet, K_TypePartition]]
