{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Union where

import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports

instance Generate a => Generate (OpUnion a) where

  -- Idea to use GLiteralTypes with giveOnly with all the constants as well
  -- since this not give use a top level onstant

  give GNone = do
      ty <- give (GOnlyTopLevel [K_TypeSet, K_TypeMSet, K_TypeFunction, K_TypeRelation])
      give (GType ty)

  give ty@GType{} = pure OpUnion <*> give ty <*> give ty
  give t          = giveUnmatched "Generate OpUnion" t

  possiblePure _ ty _ | not (allow ty) = False
    where
      allow :: Type -> Bool
      allow TypeAny        = True
      allow TypeSet{}      = True
      allow TypeMSet{}     = True
      allow TypeFunction{} = True
      allow TypeRelation{} = True
      allow _              = False

  possiblePure _ ty d   = depthOf ty + 1 <= (fromIntegral d)
  possibleNoType _ d = (2 :: Integer) <= (fromIntegral d)
