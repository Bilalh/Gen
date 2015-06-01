{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Supset where

import Conjure.Language.Expression.Op
import Gen.Essence.Id
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports

import qualified Gen.Essence.Data.Types as Types


instance Generate a => Generate (OpSupset a) where
  give GNone = give (GType TypeBool)

  give (GType TypeBool) = do
    ty <- GType <$> give (GOnlyTopLevel Types.unionLike)
    pure OpSupset <*>  give ty <*> give ty

  give t = giveUnmatched "Generate OpSupset" t

  possiblePure _ (Just TypeBool ) d = d >= 1
  possiblePure _ Just{} _           = False
  possiblePure _ _ d                = d >= 1

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _         = [RAny Types.unionLike]
