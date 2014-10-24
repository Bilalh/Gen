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
import TestGen.Arbitrary.SizeOf
import TestGen.Arbitrary.Domain

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


setLit :: SpecState -> Gen Expr
setLit s@SS{..} = do
    innerType <- atype s{depth_ = depth_ -1 }
    setLitOf s{depth_=depth_-1} innerType

--FIXME  Make the values in the set possibly unique
setLitOf :: SpecState -> Type ->  Gen Expr
setLitOf s@SS{..} innerType = do
    exprs <- listOfB 0 (min 15 (2 * depth_) ) ( exprOf s{depth_=depth_ - 1} innerType)
    return $ ELit $ ESet $ map EExpr $ exprs


msetLit :: SpecState -> Gen Expr
msetLit s@SS{..} = do
    innerType <- atype s{depth_ = depth_ -1 }
    msetLitOf s{depth_=depth_-1} innerType

msetLitOf :: SpecState -> Type ->  Gen Expr
msetLitOf s@SS{..} innerType = do
    exprs <- listOfB 0 (min 15 (2 * depth_) ) ( exprOf s{depth_=depth_ - 1} innerType)
    return $ ELit $ EMSet $ map EExpr $ exprs


matrixLitOf :: SpecState -> Type -> Gen Expr
matrixLitOf s@SS{..} innerType = do
    idx <- intDom (depth_ - 1)
    let numElems = sizeOf idx
    exprs <- vectorOf (fromInteger numElems) ( exprOf s{depth_=depth_ - 1} innerType)
    return $ ELit $ EMatrix (map EExpr $ exprs) idx

-- FIXME from mappings should be distinct?
funcLitOf :: SpecState -> Type -> Type -> Gen Expr
funcLitOf s@SS{..} fromType toType = do
    numElems <- choose (1, min 15 (2 * depth_) )
    froms <- vectorOf ( numElems)  ( exprOf s{depth_=depth_ - 1} fromType)
    tos   <- vectorOf ( numElems)  ( exprOf s{depth_=depth_ - 1} toType)
    let vs = zipWith (\a b -> (EExpr $ a, EExpr $ b) ) froms tos

    return $ ELit $ EFunction vs

relLitOf :: SpecState -> [Type] -> Gen Expr
relLitOf s@SS{..} types = do
    parts <-  mapM mkParts types
    return $ ELit $ ERelation parts

    where
        mkParts ty = do
            numElems <- choose (1, min 15 (2 * depth_) )
            vs <- vectorOf numElems ( exprOf s{depth_=depth_ - 1} ty)
            return $ map EExpr vs
