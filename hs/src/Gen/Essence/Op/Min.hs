{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Min (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports
import Gen.Essence.Domain ()
import Gen.Essence.EvalToInt


instance (Generate a, ExpressionLike a, EvalToInt a, WrapConstant a)
         => Generate (OpMin a) where
  give (GType TypeInt) = do
    d <- gets depth
    defs <- getPossibilities d [
        (return . (>=1), (K_TypeSet,    give (GType $ TypeSet  TypeInt))  )
      , (return . (>=1), (K_TypeMSet,   give (GType $ TypeMSet TypeInt))  )
      , (return . (>=1), (K_TypeMatrix, give (GType $ TypeMatrix TypeInt TypeInt))  )
      ]
    picked <- frequency3 defs
    return $ OpMin picked

  give t = giveUnmatched "Generate OpMin" t

  possiblePure _ (Just TypeInt) d = d >= 1
  possiblePure _ Just{} _         = False
  possiblePure _ Nothing _        = False

  requires _ (Just TypeInt) =
     [RAny [K_TypeSet, K_TypeMSet, K_TypeMatrix],  RAll [K_TypeInt] ]
  requires _ _  = []
