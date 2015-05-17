{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Domain where

import Conjure.Language.Domain
import Gen.Essence.Ints
import Gen.Essence.Range
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type        ()
import Gen.Imports
import Gen.Essence.EvalToInt


instance (Generate a, WrapConstant a, EvalToInt a) => Generate (Domain () a) where
  give GNone = give GNone >>= \ty -> give (GType ty)

  give (GType TypeBool)     = pure DomainBool
  give (GType TypeInt)      = DomainInt <$> vectorOf3 2 (give GNone)
  give (GType (TypeSet ty)) = DomainSet <$> pure () <*>  give GNone <*> give (GType ty)


  give t = giveUnmatched "Generate (Domain () a)" t

  possiblePure _ _ _ = True
  possibleNoType _ _ = True


instance (Generate a, WrapConstant a, EvalToInt a) => Generate (SetAttr a) where
  give GNone         = SetAttr <$> give (GNone)
  give t             = giveUnmatched "Generate (SetAttr a)" t
  possiblePure _ _ _ = True
  possibleNoType _ _ = True


instance (Generate a, WrapConstant a, EvalToInt a) => Generate (SizeAttr a)  where
  give GNone = do
    let defs =
            [ (K_SizeAttr_None, pure SizeAttr_None)
            , (K_SizeAttr_Size,    SizeAttr_Size    <$> give (GType TypeInt))
            , (K_SizeAttr_MinSize, SizeAttr_MinSize <$> give (GType TypeInt))
            , (K_SizeAttr_MaxSize, SizeAttr_MaxSize <$> give (GType TypeInt))
            , (K_SizeAttr_MinMaxSize, (uncurry SizeAttr_MinMaxSize )  <$> minMax)
            ]

    parts <- getWeights defs
    frequency3 parts

    where
    minMax = do
      (IntAsc a b) <- give GNone
      return $ (a,b)

  give t = giveUnmatched "Generate (SetAttr a)" t

  possiblePure _ _ _ = True
  possibleNoType _ _ = True
