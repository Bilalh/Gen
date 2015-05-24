{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Inverse (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpInverse a) where
  give (GType TypeBool) = do
    fTy@(TypeFunction a b) <- give (GOnlyTopLevel [K_TypeFunction])
    OpInverse <$> give (GType fTy) <*> give (GType $ TypeFunction b a)

  give t = giveUnmatched "Generate OpInverse" t

  possiblePure _ (Just TypeBool) d = d >= 1
  possiblePure _ Just{} _          = False
  possiblePure _ Nothing _         = False

  requires _ (Just TypeBool) = [RAll [K_TypeFunction]]
  requires _ _         = []
