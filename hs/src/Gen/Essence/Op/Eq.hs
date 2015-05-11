{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Eq where

import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance Generate a => Generate (OpEq a) where
  give GNone = give (GType TypeBool)

  give (GType TypeBool) = do
    ty <- GType <$> give GNone
    pure OpEq <*> give ty <*> give ty

  give t = giveUnmatched "Generate OpEq" t


  possiblePure a TypeBool d = possibleNoType a d
  possiblePure _ _ _ = False
  possibleNoType _ d = (1 :: Integer) <= (fromIntegral d)
