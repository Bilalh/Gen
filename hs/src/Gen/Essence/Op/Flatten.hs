{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Flatten (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.Id
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports
import Gen.Helpers.SizeOf

instance (Generate a, ExpressionLike a) => Generate (OpFlatten a) where
  give (GType (TypeMatrix TypeInt inn )) | notMatrix inn  = do
    d <- gets depth
    n <- choose3 (0,d-1)

    let wrap = foldr (flip (.)) (TypeMatrix TypeInt)
             $ replicate n (TypeMatrix TypeInt)
    OpFlatten <$> give (GType $ wrap inn)

  give t = giveUnmatched "Generate OpFlatten" t

  possiblePure _ (Just (TypeMatrix TypeInt inn )) d | notMatrix inn  =
    fromIntegral d >= depthOf inn + 1

  possiblePure _ (Just _ ) _ = False
  possiblePure _ Nothing _   = False

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _         = []


notMatrix :: Type -> Bool
notMatrix TypeMatrix{} = False
notMatrix _            = True
