{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Domain where

import Language.E
import AST.Imports
import TestGen.Arbitrary.Helpers
import TestGen.Arbitrary.Data
import TestGen.Arbitrary.Type

import Test.QuickCheck

import Text.Groom(groom)

import qualified Data.Text as T
import qualified Data.Map as M



dom :: Depth -> Gen (Domain)
dom 0 = oneof [ intDom 0 , return DBool ]
dom n = oneof
    [
      intDom n
    , return DBool
    , setDom n
    ]

intDom :: Depth -> Gen Domain
intDom d = return DInt `ap` (listOfB 1 4 (range d))

setDom :: Depth -> Gen Domain
setDom depth = do
    inner <- dom (depth - 1)
    return $ dset{inner}


range :: Depth -> Gen (Range Expr)
range _ = oneof
    [
      arbitrarySingle
    , arbitraryFromTo
    ]

    where
    arbitrarySingle :: Gen (Range Expr)
    arbitrarySingle = do
        a <- choose ((-10),10 :: Integer)
        return $ RSingle (ELit . EI $ a)

    arbitraryFromTo :: Gen (Range Expr)
    arbitraryFromTo = do
        do
            a <- choose ((-10),10 :: Integer)
            b <- choose (a,10)
            return $ RFromTo (ELit . EI $ a) (ELit . EI $  b)
