{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.PreImage (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.Id
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpPreImage a) where
  give (GType (TypeSet from)) = do
    to <- dgive GNone
    OpPreImage <$> give (GType $ TypeFunction from to) <*> give (GType to)

  give t = giveUnmatched "Generate OpPreImage" t

  possiblePure _ (Just (TypeSet ty)) d = fromIntegral d >= depthOf ty + 1
  possiblePure _ Just{} _    = False
  possiblePure _ Nothing _   = False

  requires _ (Just ty@TypeSet{}) = [RAll $ keyList ty, RAll [K_TypeFunction]]
  requires _ _         = []
