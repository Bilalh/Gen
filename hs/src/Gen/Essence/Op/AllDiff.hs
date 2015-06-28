{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.AllDiff where

import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports

instance Generate a => Generate (OpAllDiff a) where
  give (GType TypeBool) = do
      ty <- give (GOnlyTopLevel [K_TypeMatrix] )
      pure OpAllDiff <*> give (GType ty)

  give t = giveUnmatched "Generate OpAllDiff" t

  possiblePure _ (Just TypeBool ) d = d >= 1
  possiblePure _ _ _                = False

  requires _ _ = [RAll $ [K_TypeMatrix]]
