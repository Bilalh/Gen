{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Union where

import Conjure.Language.Expression.Op
import Gen.Essence.Id
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports

import qualified Gen.Essence.Data.Types as Types


instance Generate a => Generate (OpUnion a) where
  give GNone = do
    ty <- give (GOnlyTopLevel Types.unionLike)
    give (GType ty)

  give ty@GType{} = pure OpUnion <*> give ty <*> give ty
  give t          = giveUnmatched "Generate OpUnion" t

  possiblePure _ (Just ty) _ | not (Types.isUnionLike ty) = False
  possiblePure _ (Just ty) d = depthOf ty + 1 <= (fromIntegral d)
  possiblePure _ _ d         = d >=2

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _         = [RAny Types.unionLike]
