{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Defined (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports

instance (Generate a, ExpressionLike a) => Generate (OpDefined a) where
  give GNone = give . GType =<< give (GOnlyTopLevel [K_TypeSet])

  give (GType (TypeSet inn) ) = do
    arb <- dgive GNone
    OpDefined <$> give (GType $ TypeFunction inn arb)

  give t = giveUnmatched "Generate OpDefined" t

  possiblePure _ (Just (TypeSet inn))  d =
      (fromIntegral d) + 1 >= depthOf inn

  possiblePure _ Just{} _ = False
  possiblePure _ _ d      = d>=1

  requires _ _ = [RAll $ [K_TypeSet], RAny [K_TypeFunction]]
