{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Intersect where

import Conjure.Language.Expression.Op
import Gen.Essence.Id
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports

import qualified Gen.Essence.Data.Types as Types


instance Generate a => Generate (OpIntersect a) where
  give GNone = give . GType =<< give (GOnlyTopLevel Types.unionLike)

  give ty@GType{} = pure OpIntersect <*> give ty <*> give ty
  give t          = giveUnmatched "Generate OpIntersect" t

  possiblePure _ (Just ty) _ | not (Types.isUnionLike ty) = False
  possiblePure _ (Just ty) d = fromIntegral d >=  depthOf ty
  possiblePure _ _ d         = d >=1

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _         = [RAny Types.unionLike]
