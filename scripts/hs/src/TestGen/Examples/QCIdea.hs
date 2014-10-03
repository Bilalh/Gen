{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
module TestGen.Examples.QCIdea where

import Common.Helpers
import TestGen.ToEssence
import Language.E hiding(trace)
import Debug.Trace(trace)

import Test.QuickCheck
import Control.Monad(liftM2)
import TestGen.EssenceConstraints

import qualified Data.Text as T



data Age = Age Int
    deriving(Show)

instance Arbitrary Age where
    arbitrary = Age `liftM`  choose (0,100)

type Name    = String
data Person  = Person Name Age
             deriving (Show)

instance Arbitrary Person where
  arbitrary  = liftM2 Person arbitrary arbitrary


fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

smallNonNegativeIntegers :: Gen Int
smallNonNegativeIntegers = choose (0, 500)

prop_Fibonacci :: Property
prop_Fibonacci =
  forAll smallNonNegativeIntegers $ \n ->
    let x = fibs !! (n)
        y = fibs !! (n+1)
        z = fibs !! (n+2)
    in x + y == z

prop_Idempotent xs =
  classify (length xs < 2) "trivial" $
    sort (sort xs) == sort xs


arbTree :: Arbitrary a => Int -> Gen (Tree a)
arbTree 0 = liftM L arbitrary
arbTree n = trace (show n) $ frequency [
     (1, liftM L arbitrary)
    ,(2, liftM2 T (arbTree (n `div` 2)) (arbTree (n `div` 2)) )
    ]

data Tree a = L a | T (Tree a) (Tree a) deriving(Show)

instance Arbitrary a =>  Arbitrary (Tree a)
    where

-- equal
    -- arbitrary = oneof [
    --       liftM L arbitrary,
    --       liftM2 T arbitrary arbitrary
    --       ]

-- weighted
    -- arbitrary = frequency [
    --     (7, liftM L arbitrary),
    --     (1, liftM2 T arbitrary arbitrary)
    --     ]

-- sized
    arbitrary = sized arbTree


newtype SmallIntList = SmallIntList [Int] deriving (Eq,Show)

instance Arbitrary SmallIntList where
  arbitrary = sized $ \s -> do
                 n <- choose (0,s `min` 50)
                 xs <- vectorOf n (choose (-10000,10000))
                 return (SmallIntList xs)
  shrink (SmallIntList xs) = map SmallIntList (shrink xs)


