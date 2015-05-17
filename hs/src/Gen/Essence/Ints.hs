{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Ints where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Gen.Essence.Range
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type        ()
import Gen.Imports
import Gen.Essence.EvalToInt

import qualified Data.Set as S


data IntAsc a = IntAsc a a
 deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Pretty a => Pretty (IntAsc a) where
    pretty (IntAsc a b) = "IntAsc" <+> pretty a <+> pretty b

instance  (EvalToInt a, Generate a, WrapConstant a) => Generate (IntAsc a) where
    give GNone = do
      d <-  gets depth
      if d < 2 then do
         a <- chooseInt
         b <- intLowerBounded a
         return $ IntAsc (wrapConstant . ConstantInt $ a)
                         (wrapConstant . ConstantInt $ b)
      else do
         a0 <- give (GType TypeInt)
         b0 <- give (GType TypeInt)
         a  <- ensureGTE0 a0
         b  <- ensureGTE0 b0
         ae <- evalToInt a
         be <- evalToInt b

         let (ma,mb) = if ae < be then (a,b) else (b,a)
         return $ IntAsc ma mb


    give t       = giveUnmatched "Generate Var" t
    possible _ _ = return True


intDomainOfSize :: forall a . WrapConstant a
               => Integer -> GenSt (Domain () a)
intDomainOfSize numElems = do
  numRanges <- choose3 (1 :: Integer, numElems)
  ranges <- mkRanges numElems numElems numRanges S.empty
  -- let idx = DomainInt (sortBy  rangeComp ranges)
  let idx = DomainInt ranges
  return idx
