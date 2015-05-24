{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Participants (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.Id
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpParticipants a) where
  give (GType (TypeSet inn)) = do
    OpParticipants <$> give (GType (TypePartition inn))

  give t = giveUnmatched "Generate OpParticipants" t

  possiblePure _ (Just (TypeSet ty)) d = fromIntegral d >= depthOf ty + 1
  possiblePure _ Just{} _    = False
  possiblePure _ Nothing _   = False

  requires _ (Just ty@TypeSet{}) = [RAll $ keyList ty, RAll [K_TypePartition]]
  requires _ _         = []
