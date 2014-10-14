{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Op where

import Language.E
import AST.Imports
import TestGen.Arbitrary.Helpers
import TestGen.Arbitrary.Data
import TestGen.Arbitrary.Type

import TestGen.Arbitrary.Expr

import Test.QuickCheck
import Text.Groom(groom)
import qualified Data.Text as T
import qualified Data.Map as M


type Bop = (Expr -> Expr -> BinOp)

bop :: SpecState -> Bop ->  Gen Expr
bop s@SS{..} op =  do
    exprType <- atype s{depth_=depth_ - 1}

    e1 <- exprOf s{depth_=depth_ - 1} exprType
    e2 <- exprOf s{depth_=depth_ - 1} exprType

    return $ EBinOp $  op e1 e2


bopOf :: SpecState -> Bop -> Type -> Gen Expr
bopOf s@SS{..} op exprType =  do
    e1 <- exprOf s{depth_=depth_ - 1} exprType
    e2 <- exprOf s{depth_=depth_ - 1} exprType

    return $ EBinOp $ op e1 e2

opOf :: SS -> (Expr -> UniOp) -> Type ->  Gen Expr
opOf s@SS{..} op exprType = do
    e1 <- exprOf s{depth_=depth_ - 1} exprType
    return $ EUniOp $ op e1

bar :: SS -> Gen Expr
bar s@SS{..} = do
    exprType <- undefined :: Gen Type
    opOf s UBar exprType
