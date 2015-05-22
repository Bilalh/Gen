{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Flatten (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.Id
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports

instance (Generate a, ExpressionLike a) => Generate (OpFlatten a) where
  give GNone = do
    ty <- withWeights [(K_TypeMatrix, 0)] $ give GNone
    give (GType $ TypeMatrix TypeInt ty)

  give (GType (TypeMatrix TypeInt inn )) | notMatrix inn  = do
    sanity "Op Flatten"
    d <- gets depth
    n <- choose3 (1,d-1)

    let wrap = foldr (flip (.)) id  $ replicate n (TypeMatrix TypeInt)
    OpFlatten <$> give (GType $ wrap inn)

  give t = giveUnmatched "Generate OpFlatten" t

  possiblePure _ (Just (TypeMatrix TypeInt inn )) d | notMatrix inn  = d >= 1

  possiblePure _ (Just _ ) _ = False
  possiblePure _ Nothing d   = d >=1

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _         = [RAll [K_TypeMatrix] , RAny [K_TypeInt] ]


notMatrix :: Type -> Bool
notMatrix TypeMatrix{} = False
notMatrix _            = True
