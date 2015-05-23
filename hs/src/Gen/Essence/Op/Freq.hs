{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Freq (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports

instance (Generate a, ExpressionLike a) => Generate (OpFreq a) where
  give GNone = give (GType TypeInt)

  give (GType TypeInt) = do
    m@(TypeMSet x) <- give (GOnlyTopLevel [K_TypeMSet])
    OpFreq <$> give (GType m) <*> give (GType x)

  give t = giveUnmatched "Generate OpFreq" t

  possiblePure _ (Just TypeInt) d = d >=1
  possiblePure _ (Just _ ) _      = False
  possiblePure _ Nothing d        = d >= 1

  requires _ _ = [RAny [K_TypeInt, K_TypeBool], RAll [K_TypeSet] ]
