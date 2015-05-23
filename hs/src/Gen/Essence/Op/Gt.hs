{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Gt where

import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports

import qualified Gen.Essence.Data.Types as Types


instance Generate a => Generate (OpGt a) where
  give GNone = give (GType TypeBool)

  give (GType TypeBool) = do
    ty <- GType <$> giveOnly GNone Types.ordered
    pure OpGt <*>  (give ty) <*> give ty

  give t = giveUnmatched "Generate OpGt" t

  possiblePure _ (Just TypeBool ) _ = True
  possiblePure _ Just{} _           = False
  possiblePure _ _ d                = d >= 0

  requires _ _ = [RAny Types.ordered]
