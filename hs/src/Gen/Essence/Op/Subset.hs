{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Subset where

import Conjure.Language.Expression.Op
import Gen.Essence.Id
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports

import qualified Gen.Essence.Data.Types as Types


instance Generate a => Generate (OpSubset a) where
  give GNone = give (GType TypeBool)

  give (GType TypeBool) = do
    ty <- GType <$> give (GOnlyTopLevel Types.unionLike)
    pure OpSubset <*>  give ty <*> give ty

  give t = giveUnmatched "Generate OpSubset" t

  possiblePure _ (Just ty)  _ | ty /= TypeBool = False
  possiblePure _ _ d = d >= 1

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _ = [RAny Types.unionLike]
