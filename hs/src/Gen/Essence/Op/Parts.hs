{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Parts (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.Id
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpParts a) where
  give (GType (TypeSet (TypeSet inn))) = do
    OpParts <$> give (GType (TypePartition inn))

  give t = giveUnmatched "Generate OpParts" t

  possiblePure _ (Just (TypeSet (TypeSet ty))) d = fromIntegral d >= depthOf ty + 2
  possiblePure _ Just{} _    = False
  possiblePure _ Nothing _   = False

  -- Commonly  [RAll $ keyList ty]
  requires _ (Just ty@(TypeSet TypeSet{}) ) = [RAll $ keyList ty, RAll [K_TypePartition]]
  requires _ _         = []
