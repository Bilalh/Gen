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
  give (GType ty@(TypeMatrix TypeInt inn )) | notMatrix inn  = do
    d <- gets depth
    n <- choose3 (0, d - (fromInteger $ depthOf ty)  )

    let wrap = foldr (flip (.)) id $ replicate n (TypeMatrix TypeInt)
    logInfo2 $line [ nn "ret" (ty)
                   , nn "ret depth" (depthOf $ ty)
                   , nn "depth" d
                   , nn "n" n
                   , nn "wrapped" (wrap ty)
                   , nn "wrapped depth" ( depthOf $ wrap ty)
                   , nn "wrapped Groom" ( groom $ wrap ty)
                   ]

    OpFlatten <$> give (GType $ wrap ty)

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
