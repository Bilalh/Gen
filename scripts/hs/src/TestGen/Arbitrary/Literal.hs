{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}

module TestGen.Arbitrary.Literal where

import AST.Imports
import TestGen.Arbitrary.Helpers
import TestGen.Arbitrary.Data
import TestGen.Arbitrary.Type

import TestGen.Arbitrary.Expr(exprOf)

import Test.QuickCheck

import qualified Data.Text as T
import qualified Data.Map as M
import Text.Groom(groom)

boolLit :: SpecState -> Gen Expr
boolLit _ = do
    b <- arbitrary
    return  (ELit (EB b))

intLit :: SpecState -> Gen Expr
intLit _ = do
    i <- choose ((-10),10 :: Integer)
    return (ELit (EI i) )

--FIXME depth?
setLit :: SpecState -> Gen Expr
setLit s@SS{..} = do
    innerType <- atype s{depth_ = depth_ -1 }
    setLitOf s{depth_=depth_-1} innerType

setLitOf :: SpecState -> Type ->  Gen Expr
setLitOf s@SS{..} innerType = do
    exprs <- listOfB 0 15 ( exprOf s{depth_=depth_ - 1} innerType)
    return $ ELit $ ESet $ map EExpr $ exprs
