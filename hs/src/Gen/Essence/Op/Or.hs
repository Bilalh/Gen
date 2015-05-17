{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Or where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpOr a) where
  give GNone = give (GType TypeBool)

  give (GType TypeBool) = do
      OpOr <$> give  (GType $ TypeMatrix TypeInt TypeBool)


  give t = giveUnmatched "Generate OpOr" t

  possiblePure _ TypeBool d =  d >= 2
  possiblePure _ _ _ = False

  possibleNoType _ d = (2 :: Integer) <= (fromIntegral d)
