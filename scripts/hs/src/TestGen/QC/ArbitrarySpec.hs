{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module TestGen.QC.ArbitrarySpec where

import Language.E hiding(trace)

import AST.Constraint
import AST.Domain
import AST.Literal
import AST.SpecE




import Test.QuickCheck
import Control.Monad(liftM2)
import qualified Data.Text as T

import qualified Data.Map as M


instance Arbitrary SpecE where
    arbitrary = arbitrarySpec


arbitrarySpec :: Gen SpecE
arbitrarySpec = do
    doms <- listOf1 arbitraryDom
    let withNames =  zipWith (\d i -> (name i , Find d)) doms [1 :: Int ..]

    exprs <- listOf arbitraryExpr
    let mappings = M.fromList withNames

    return $ SpecE mappings  (ELit (EB True) :  exprs)

    where name i =  T.pack $  "var" ++  (show  i)


arbitraryDom ::  Gen (Domain)
arbitraryDom =oneof
    [
        return DInt `ap` arbitrary
    ]

arbitraryExpr :: Gen Expr
arbitraryExpr = elements [ ELit (EB True), ELit (EB False)  ]


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
    arbitrarySized n = oneof [
         liftM EB arbitrary
        ,liftM EI arbitrary
        ,liftM ESet arbitrary
        ]


instance Arbitrary Text where
    arbitrary = liftM (T.pack . ("var_" ++) .  show) $ choose (10,99 :: Integer)

instance Arbitrary (Range Integer) where
    arbitrary = oneof
        [
          liftM RSingle arbitrary
        , liftM2 RFromTo arbitrary arbitrary
        ]

    shrink x = genericShrink x
