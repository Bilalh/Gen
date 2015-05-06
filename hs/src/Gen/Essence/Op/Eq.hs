{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Eq where

import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Helpers.StandardImports


instance Generate a => Generate (OpEq a) where
  give GNone = give (GType TBool)

  give (GType TBool) = do
    ty <- GType <$> give GNone
    pure OpEq <*> give ty <*> give ty

  give t = giveUnmatched "Generate OpEq" t

  -- Returns True if this op can be used with the specified return type
  -- and the remaing depth. This Op is counted in the depth calculation
  possiblePure _ ty _ | ty /= TBool = False
  possiblePure _ ty d = depthOf ty + 1 <= (fromIntegral d)
