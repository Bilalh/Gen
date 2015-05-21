{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Max (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports
import Gen.Essence.Domain ()
import Gen.Essence.EvalToInt


instance (Generate a, ExpressionLike a, EvalToInt a, WrapConstant a)
         => Generate (OpMax a) where
  give GNone = do
    -- ty <- giveOnly GNone [K_TypeBool, K_TypeInt]
    ty <- giveOnly GNone [K_TypeInt]
    give (GType ty)

  -- give ty@(GType TypeBool)  = do
  --   dom :: Domain () a <- give ty
  --   return $ OpMax (wrapDomain dom)

  give (GType TypeInt) = do
    d <- gets depth
    defs <- getPossibilities d [
        -- (return . (>=1), (K_DomainInt,  wrapDomain <$> dgive ty) )
        (return . (>=1), (K_TypeSet,    give (GType $ TypeSet  TypeInt))  )
      , (return . (>=1), (K_TypeMSet,   give (GType $ TypeMSet TypeInt))  )
      , (return . (>=1), (K_TypeMatrix, give (GType $ TypeMatrix TypeInt TypeInt))  )
      ]
    freqError $line defs
    picked <- frequency3 defs
    return $ OpMax picked

  give t = giveUnmatched "Generate OpMax" t

  -- possiblePure _ (Just ty) d | ty `elem` [TypeBool, TypeInt] = d >= 1
  possiblePure _ (Just ty) d | ty `elem` [TypeInt] = d >= 1
  possiblePure _ _ _  = False

  -- requires _ (Just TypeBool) = [RAll [K_DomainBool] ]
  requires _ (Just TypeInt) =
     -- [RAny [K_TypeSet, K_TypeMSet, K_TypeMatrix, K_DomainInt],  RAll [K_TypeInt] ]
     [RAny [K_TypeSet, K_TypeMSet, K_TypeMatrix],  RAll [K_TypeInt] ]
  requires _ _  = []
