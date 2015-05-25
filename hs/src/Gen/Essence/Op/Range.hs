{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Range (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.Id
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports

instance (Generate a, ExpressionLike a) => Generate (OpRange a) where
  give (GType (TypeSet ty)) = do
    from <- dgive GNone
    OpRange <$> (give (GType $ TypeFunction from ty))

  give t = giveUnmatched "Generate OpRange" t

  possiblePure _ (Just (TypeSet ty)) d = fromIntegral d >= depthOf ty + 1
  possiblePure _ Just{} _    = False
  possiblePure _ Nothing _   = False

  requires _ (Just ty) = [RAll $ keyList ty, RAll [K_TypeFunction] ]
  requires _ _         = []
