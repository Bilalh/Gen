{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}

module TestGen.Arbitrary.Literal where

import TestGen.Arbitrary.Helpers.Prelude

import TestGen.Arbitrary.Type
import TestGen.Arbitrary.SizeOf
import TestGen.Arbitrary.Domain

import TestGen.Arbitrary.Expr(exprOf)

-- FIXME pick only values that in the domain?


boolLit :: GG Expr
boolLit = do
    b <- lift arbitrary
    return  (ELit (EB b))

intLit :: GG Expr
intLit  = do
    i <- choose2 ((-10),10 :: Integer)
    return (ELit (EI i) )


setLit :: GG Expr
setLit = do
    innerType <- withDepthDec atype
    withDepthDec (setLitOf innerType)

--FIXME  Make the values in the set possibly unique
setLitOf :: Type ->  GG Expr
setLitOf innerType = do
    depth_ <- gets depth_
    exprs <- listOfBounds (0,  min 15 (2 * depth_) ) (withDepthDec $ exprOf innerType)
    return $ ELit $ ESet $ map EExpr $ exprs


msetLit :: GG Expr
msetLit = do
    innerType <-  withDepthDec atype
    withDepthDec (msetLitOf innerType)

msetLitOf :: Type ->  GG Expr
msetLitOf innerType = do
    depth_ <- gets depth_
    exprs <- listOfBounds (0,  min 15 (2 * depth_) ) (withDepthDec $ exprOf innerType)
    return $ ELit $ EMSet $ map EExpr $ exprs


matrixLitOf :: Type -> GG Expr
matrixLitOf innerType = do
    depth_ <- gets depth_
    idx <- lift $ intDom (depth_ - 1)
    let numElems = sizeOf idx
    exprs <- vectorOf2 (fromInteger numElems) ( withDepthDec $ exprOf innerType)
    return $ ELit $ EMatrix (map EExpr $ exprs) idx

-- FIXME from mappings should be distinct?
funcLitOf :: Type -> Type -> GG Expr
funcLitOf fromType toType = do
    depth_ <- gets depth_
    numElems <- choose2 (1, min 15 (2 * depth_) )
    froms <- vectorOf2 numElems  ( withDepthDec $ exprOf fromType)
    tos   <- vectorOf2 numElems  ( withDepthDec $ exprOf toType)
    let vs = zipWith (\a b -> (EExpr $ a, EExpr $ b) ) froms tos

    return $ ELit $ EFunction vs


tupleLitOf :: [Type] -> GG Expr
tupleLitOf tys = do
    depth_ <- gets depth_
    if
        | depth_ < 1 -> ggError "tupleLitOf depth_ <1" [pretty $ groom tys]
        | otherwise -> do
            parts <- mapM mkParts tys
            return $ ELit $ ETuple parts

    where
        mkParts ty  = do
            e <- withDepthDec $ exprOf ty
            return $ EExpr  e

relLitOf :: [Type] -> GG Expr
relLitOf types = do
    depth_ <- gets depth_
    if
        | depth_ < 2 -> ggError "relLitOf depth_ <2" [pretty $ groom types]
        | otherwise -> do
            parts <-  vectorOf2 3 $ mkParts types
            return $ ELit $ ERelation parts

    where
    mkParts tys = do
        (ELit lit) <- withDepthDec $ tupleLitOf tys
        return lit



parLitOf :: Type -> GG Expr
parLitOf innerType = do
    depth_ <- gets depth_

    if
        | depth_ < 1 -> ggError "parLitOf depth <1" [pretty $ groom innerType]
        | otherwise -> do
            numElems <- choose2 (1, min 15 (2 * depth_) )
            parts <-  vectorOf2 numElems (mkPart innerType)
            return $ ELit $ EPartition parts

    where
        mkPart ty = do
            depth_ <- gets depth_
            numElems <- choose2 (1, min 15 (2 * depth_) )
            vs <- vectorOf2 numElems ( withDepthDec $ exprOf ty)
            return $ map EExpr vs
