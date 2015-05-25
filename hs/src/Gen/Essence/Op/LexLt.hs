{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.LexLt (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpLexLt a) where
  give (GType TypeBool) = do
    ty <- TypeMatrix TypeInt <$> dgive GNone
    OpLexLt <$> give (GType ty) <*> give (GType ty)

  give t = giveUnmatched "Generate OpLexLt" t

  possiblePure _ (Just TypeBool) d = d >=1
  possiblePure _ Just{} _          = False
  possiblePure _ Nothing _         = False

  requires _ (Just TypeBool) = [RAll [K_TypeMatrix]]
  requires _ _               = []
