{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Leq where

import Conjure.Language.Expression.Op
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Helpers.StandardImports


instance Generate a => Generate (OpLeq a) where
  give GNone = give (GType TBool)

  give (GType TBool) = do
    ws <- getWeights [ ("TInt", pure TInt)
                     , ("TBool", pure TBool)]
    ty <- GType <$> frequency3 ws
    pure OpLeq <*> give ty <*> give ty

  give t = giveUnmatched "Generate OpGeq" t

  -- Returns True if this op can be used with the specified return type
  -- and the remaing depth. This Op is counted in the depth calculation
  possiblePure _ ty _ | ty /= TBool = False
  possiblePure _ ty d = depthOf ty + 1 <= (fromIntegral d)