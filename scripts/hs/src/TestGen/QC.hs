{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}

module TestGen.QC where

import Common.Helpers
import TestGen.ToEssence
import Language.E hiding(trace)
import Debug.Trace(trace)
import Language.E.Pipeline.ReadIn(readSpecFromFile)

import Control.Monad(liftM2)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import qualified Test.QuickCheck.Property as P
import TestGen.EssenceConstraints
import TestGen.Runner(SettingI(..))
import TestGen.TestGen(runRefine')
import qualified Data.Text as T

instance Arbitrary Text where
    arbitrary = liftM (T.pack . ("var_" ++) .  show) $ choose (10,99 :: Integer)

instance Arbitrary Eexpr where
    arbitrary = sized arbitrarySized

class Arbitrary a => ArbitrarySized a where
    arbitrarySized :: Int ->  Gen a
    arbitrarySized _ = error "no default sized generator"

instance Arbitrary a => ArbitrarySized [a] where
  arbitrarySized n  = do
       k <- choose (0,n)
       sequence [ arbitrary | _ <- [1..k] ]


instance ArbitrarySized Eexpr where
    arbitrarySized 0 = oneof [
         liftM Evar (arbitrary)
        ,liftM Elit (arbitrarySized 0)
        ]
    arbitrarySized n = oneof [
            liftM  Evar (arbitrary)
           ,liftM2 Egt  (arbitrarySized ((n-1) `div` 2)) (arbitrarySized ((n-1) `div` 2))
           ,liftM2 Eneq (arbitrarySized ((n-1) `div` 2)) (arbitrarySized ((n-1) `div` 2))
           ,liftM  Elit (arbitrarySized (n-1))
        ]

instance Arbitrary EssenceLiteral where
    arbitrary = sized arbitrarySized

instance ArbitrarySized EssenceLiteral where
    arbitrarySized 0 = oneof [
         liftM ELB arbitrary
        ,liftM ELI arbitrary
        ]
    arbitrarySized n = oneof [
         liftM ELB arbitrary
        ,liftM ELI arbitrary
        ,liftM ELSet arbitrary
        ]

prop_consts :: Eexpr -> Property
prop_consts es = es === es

specc = readSpecFromFile "/Users/bilalh/Desktop/Results/testgen/Older/aaaa/_dss 01.27.30/1406333220/1406333220.essence"

instance Arbitrary Spec where
    arbitrary = elements [speccc]

prop_specs_refine :: Spec -> Property
prop_specs_refine spec =
    monadicIO $ do
        (ts, result) <- run $ runRefine' spec "__" 4
        fail ts

runn = do
    sp <- specc
    runRefine' sp "__"  4


infix 4 /==
(/==) :: (Eq a, Show a) => a -> a -> Property
x /== y =
  counterexample (show x ++ " == " ++ show y) (x /= y)



speccc = Spec (
    LanguageVersion "Essence" [1,3]) (StatementAndNext (Tagged (Tag "topLevel") [Tagged (Tag "declaration") [Tagged (Tag "find") [Tagged (Tag "name") [Tagged (Tag "reference") [Prim (S "var0")]],Tagged (Tag "domain") [Tagged (Tag "domain") [Tagged (Tag "function") [Tagged (Tag "attributes") [Tagged (Tag "attrCollection") [Tagged (Tag "attribute") [Tagged (Tag "name") [Tagged (Tag "reference") [Prim (S "total")]]],Tagged (Tag "attribute") [Tagged (Tag "name") [Tagged (Tag "reference") [Prim (S "surjective")]]]]],Tagged (Tag "innerFrom") [Tagged (Tag "domain") [Tagged (Tag "function") [Tagged (Tag "attributes") [Tagged (Tag "attrCollection") [Tagged (Tag "attribute") [Tagged (Tag "name") [Tagged (Tag "reference") [Prim (S "total")]]],Tagged (Tag "attribute") [Tagged (Tag "nameValue") [Tagged (Tag "name") [Tagged (Tag "reference") [Prim (S "minSize")]],Tagged (Tag "value") [Tagged (Tag "value") [Tagged (Tag "literal") [Prim (I 1)]]]]],Tagged (Tag "attribute") [Tagged (Tag "name") [Tagged (Tag "reference") [Prim (S "injective")]]],Tagged (Tag "attribute") [Tagged (Tag "nameValue") [Tagged (Tag "name") [Tagged (Tag "reference") [Prim (S "size")]],Tagged (Tag "value") [Tagged (Tag "value") [Tagged (Tag "literal") [Prim (I 3)]]]]]]],Tagged (Tag "innerFrom") [Tagged (Tag "domain") [Tagged (Tag "int") [Tagged (Tag "ranges") [Tagged (Tag "range") [Tagged (Tag "fromTo") [Tagged (Tag "value") [Tagged (Tag "literal") [Prim (I 3)]],Tagged (Tag "value") [Tagged (Tag "literal") [Prim (I 4)]]]]]]]],Tagged (Tag "innerTo") [Tagged (Tag "domain") [Tagged (Tag "int") [Tagged (Tag "ranges") [Tagged (Tag "range") [Tagged (Tag "fromTo") [Tagged (Tag "value") [Tagged (Tag "literal") [Prim (I 2)]],Tagged (Tag "value") [Tagged (Tag "literal") [Prim (I 4)]]]]]]]]]]],Tagged (Tag "innerTo") [Tagged (Tag "domain") [Tagged (Tag "set") [Tagged (Tag "attributes") [Tagged (Tag "attrCollection") [Tagged (Tag "attribute") [Tagged (Tag "nameValue") [Tagged (Tag "name") [Tagged (Tag "reference") [Prim (S "size")]],Tagged (Tag "value") [Tagged (Tag "value") [Tagged (Tag "literal") [Prim (I 4)]]]]],Tagged (Tag "attribute") [Tagged (Tag "nameValue") [Tagged (Tag "name") [Tagged (Tag "reference") [Prim (S "minSize")]],Tagged (Tag "value") [Tagged (Tag "value") [Tagged (Tag "literal") [Prim (I 4)]]]]],Tagged (Tag "attribute") [Tagged (Tag "nameValue") [Tagged (Tag "name") [Tagged (Tag "reference") [Prim (S "maxSize")]],Tagged (Tag "value") [Tagged (Tag "value") [Tagged (Tag "literal") [Prim (I 3)]]]]]]],Tagged (Tag "inner") [Tagged (Tag "domain") [Tagged (Tag "int") [Tagged (Tag "ranges") [Tagged (Tag "range") [Tagged (Tag "fromTo") [Tagged (Tag "value") [Tagged (Tag "literal") [Prim (I 4)]],Tagged (Tag "value") [Tagged (Tag "literal") [Prim (I 5)]]]]]]]]]]]]]]]]]) EOF)
