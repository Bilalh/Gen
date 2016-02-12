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
import Gen.Essence.Id

import qualified Data.Set as S


data IntAsc a = IntAsc a a
 deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | InRanged (wraped int) (int value)
data IntRanged a = IntRanged a Integer
 deriving (Eq, Ord, Show, Data, Typeable, Generic)


instance Pretty a => Pretty (IntAsc a) where
    pretty (IntAsc a b) = "IntAsc" <+> pretty a <+> pretty b

instance Pretty a => Pretty (IntRanged a) where
    pretty (IntRanged a val) = "IntRanged" <+> pretty a <+> pretty val


instance GetKey a => GetKey (IntAsc a) where
  getKey _   = K_IntAsc
  keyTree d  = KTree (defWeight d) (getKey d) (map keyTree $ children d)

instance GetKey a => GetKey (IntRanged a) where
  getKey _   = K_IntRanged
  keyTree d  = KTree (defWeight d) (getKey d) (map keyTree $ children d)


instance  (EvalToInt a, Generate a, GenInfo a) => Generate (IntRanged a) where
  give (GIntRanged low high) = do
    d <-  gets depth
    -- 2 for because the expression may need to be adjusted
    if d < 2 then do
       v <- choose3 (low,high)
       return $ IntRanged (wrapConstant . ConstantInt $ v) v
    else withWeights [(K_EVar, 0), (K_LVar,0)] $ do
       a :: a <- give (GType TypeInt)
       uncurry IntRanged <$>  adjustInRange a low high


  give t       = giveUnmatched "Generate IntRanged a" t
  possible _ _ = return True

instance  (EvalToInt a, Generate a, GenInfo a) => Generate (IntAsc a) where
    give GNone = do
      d <-  gets depth
      if d < 2 then do
         a <- chooseInt
         b <- intLowerBounded a
         return $ IntAsc (wrapConstant . ConstantInt $ a)
                         (wrapConstant . ConstantInt $ b)

      else withWeights [(K_EVar, 0), (K_LVar,0)] $ do
           a0 :: a <- dgive (GType TypeInt)
           b0 :: a <- dgive (GType TypeInt)
           a  <- ensureGTE0 a0
           b  <- ensureGTE0 b0
           ae <- evalToInt a
           be <- evalToInt b

           let (ma,mb) = if ae < be then (a,b) else (b,a)
           logDebug2 $line [ nn "depth" d
                          , nn "a0" a0
                          , nn "b0" b0
                          , nn "a"  a
                          , nn "b"  b
                          , nn "eval a" ae
                          , nn "eval b" be
                          , nn "IntAsc" (IntAsc ma mb)
                          ]

           return $ IntAsc ma mb


    give t       = giveUnmatched "IntAsc a" t
    possible _ _ = return True


intDomainOfSize :: forall a . GenInfo a
               => Integer -> GenSt (Domain () a)
intDomainOfSize numElems = do
  numRanges <- choose3 (1 :: Integer, numElems)
  ranges <- mkRanges numElems numElems numRanges S.empty
  -- let idx = DomainInt (sortBy  rangeComp ranges)
  let idx = DomainInt ranges
  return idx
