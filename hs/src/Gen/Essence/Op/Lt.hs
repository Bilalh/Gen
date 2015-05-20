{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Lt where

import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports

import qualified Gen.Essence.Data.Types as Types


instance Generate a => Generate (OpLt a) where
  give GNone = give (GType TypeBool)

  give (GType TypeBool) = do
    ty <- GType <$> giveOnly GNone Types.ordered
    pure OpLt <*>  (give ty) <*> give ty

  give t = giveUnmatched "Generate OpLt" t

  possiblePure _ (Just ty)  _ | ty /= TypeBool = False
  possiblePure _ _ d = d >= 1

  requires _ _ = [RAny Types.ordered]
