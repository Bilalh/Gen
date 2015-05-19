{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Geq where

import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports

import qualified Gen.Essence.Data.Types as Types


instance Generate a => Generate (OpGeq a) where
  give GNone = give (GType TypeBool)

  give (GType TypeBool) = do
    ty <- GType <$> giveOnly GNone Types.ordered
    pure OpGeq <*> give ty <*> give ty

  give t = giveUnmatched "Generate OpGeq" t

  possiblePure _ (Just ty)  _ | ty /= TypeBool = False
  possiblePure _ _ d = d >= 1
