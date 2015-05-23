{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Neq where

import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance Generate a => Generate (OpNeq a) where
  give GNone = give (GType TypeBool)

  give (GType TypeBool) = do
    ty <- GType <$> give GNone
    pure OpNeq <*> give ty <*> give ty

  give t = giveUnmatched "Generate OpNeq" t


  possiblePure _ (Just TypeBool ) _ = True
  possiblePure _ Just{} _           = False
  possiblePure _ _ d                = d >= 0

  requires _ _ = []
