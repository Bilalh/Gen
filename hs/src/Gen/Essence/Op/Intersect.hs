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

  give GNone = do
      ty <- give (GOnlyTopLevel Types.unionLike)
      give (GType ty)

  give ty@GType{} = pure OpIntersect <*> give ty <*> give ty
  give t          = giveUnmatched "Generate OpIntersect" t

  possiblePure _ (Just ty) _ | not (Types.isUnionLike ty) = False
  possiblePure _ (Just ty) d = depthOf ty + 1 <= (fromIntegral d)
  possiblePure _ _ d         = d >=2

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _         = [RAny Types.unionLike]
