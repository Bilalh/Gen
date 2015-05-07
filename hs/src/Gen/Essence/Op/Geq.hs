{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Geq where

import Conjure.Language.Expression.Op
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Helpers.SizeOf
import Gen.Imports


instance Generate a => Generate (OpGeq a) where
  give GNone = give (GType TypeBool)

  give (GType TypeBool) = do
    ws <- getWeights [ ("TypeInt", pure TypeInt)
                     , ("TypeBool", pure TypeBool)]
    ty <- GType <$> frequency3 ws
    pure OpGeq <*> give ty <*> give ty

  give t = giveUnmatched "Generate OpGeq" t

  -- Returns True if this op can be used with the specified return type
  -- and the remaing depth. This Op is counted in the depth calculation
  possiblePure _ ty _ | ty /= TypeBool = False
  possiblePure _ ty d = depthOf ty + 1 <= (fromIntegral d)
