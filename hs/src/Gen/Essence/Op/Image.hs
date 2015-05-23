{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Image (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports

instance (Generate a, ExpressionLike a) => Generate (OpImage a) where
  give (GType to) = do
   from :: Type <- dgive (GNone)
   OpImage <$> give (GType $ TypeFunction from to) <*> give (GType from)

  give t = giveUnmatched "Generate OpImage" t

  possiblePure _ (Just x) d = fromIntegral d >= depthOf x + 1
  possiblePure _ Nothing _  = False

  requires _ _         = [RAll [K_TypeFunction] ]
