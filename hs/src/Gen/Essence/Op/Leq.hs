{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Leq where

import Conjure.Language.Expression.Op
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports


instance Generate a => Generate (OpLeq a) where
  give GNone = give (GType TypeBool)

  give (GType TypeBool) = do
    ws <- getWeights [ (K_TypeInt , pure TypeInt)
                     ]
    ty <- GType <$> frequency3 ws
    pure OpLeq <*> give ty <*> give ty

  give t = giveUnmatched "Generate OpGeq" t

  possiblePure _ ty _ | ty /= TypeBool = False
  possiblePure _ ty d = depthOf ty + 1 <= (fromIntegral d )
  possibleNoType _ d = (2 :: Integer) <= (fromIntegral d)
