{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module TestGen.ZOld where

import Language.E hiding(trace)

import AST.Constraint
import AST.Domain
import AST.Literal
-- import AST.ToEssence

import Test.QuickCheck
import Control.Monad(liftM2)
import qualified Data.Text as T


-- import Language.E hiding(trace)
-- import Debug.Trace(trace)

instance Arbitrary Expr where
    arbitrary = sized arbitrarySized

class Arbitrary a => ArbitrarySized a where
    arbitrarySized :: Int ->  Gen a
    arbitrarySized _ = error "no default sized generator"

instance Arbitrary a => ArbitrarySized [a] where
  arbitrarySized n  = do
       k <- choose (0,n)
       sequence [ arbitrary | _ <- [1..k] ]


instance ArbitrarySized Expr where
    arbitrarySized 0 = oneof [
         liftM EVar (arbitrary)
        ,liftM ELit (arbitrarySized 0)
        ]
    arbitrarySized n = oneof [
            liftM  EVar (arbitrary)
           ,liftM2 EGT  (arbitrarySized ((n-1) `div` 2)) (arbitrarySized ((n-1) `div` 2))
           ,liftM  ELit (arbitrarySized (n-1))
        ]

instance Arbitrary Literal where
    arbitrary = sized arbitrarySized
    -- shrink x  = genericShrink x


instance ArbitrarySized Literal where
    arbitrarySized 0 = oneof [
         liftM EB arbitrary
        ,liftM EI arbitrary
        ]
    arbitrarySized _ = oneof [
         liftM EB arbitrary
        ,liftM EI arbitrary
        ,liftM ESet arbitrary
        ]


instance Arbitrary Text where
    arbitrary = liftM (T.pack . ("var_" ++) .  show) $ choose (10,99 :: Integer)
