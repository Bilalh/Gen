{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.SupsetEq where

import Conjure.Language.Expression.Op
import Gen.Essence.Id
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports

import qualified Gen.Essence.Data.Types as Types


instance Generate a => Generate (OpSupsetEq a) where
  give GNone = give (GType TypeBool)

  give (GType TypeBool) = do
    ty <- GType <$> give (GOnlyTopLevel Types.unionLike)
    pure OpSupsetEq <*>  give ty <*> give ty

  give t = giveUnmatched "Generate OpSupsetEq" t

  possiblePure _ (Just ty)  _ | ty /= TypeBool = False
  possiblePure _ _ d = d >= 2

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _ = [RAny Types.unionLike]
