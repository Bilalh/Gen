{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Minus (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.Id
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpMinus a) where
  give ty@(GType inn) | allow inn = pure OpMinus <*> give ty <*> give ty
  give t                          = giveUnmatched "Generate OpMinus" t

  possiblePure _ (Just ty) d = allow ty && fromIntegral d >= depthOf ty
  possiblePure _ Nothing _   = False

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _         = []


allow :: Type -> Bool
allow TypeInt{}       = True
allow TypeSet{}       = True
allow TypeMSet{}      = True
allow TypeFunction{}  = True
allow TypeRelation{}  = True
allow _               = False
