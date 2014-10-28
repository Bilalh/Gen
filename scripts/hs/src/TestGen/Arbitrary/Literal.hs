{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}

module TestGen.Arbitrary.Literal where

import TestGen.Arbitrary.Helpers.Prelude

import TestGen.Arbitrary.Type
import TestGen.Arbitrary.SizeOf
import TestGen.Arbitrary.Domain

import TestGen.Arbitrary.Expr(exprOf)

-- FIXME pick only values that in the domain?


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


tupleLitOf :: SpecState -> [Type] -> Gen Expr
tupleLitOf s tys | tracef "tupleLitOf" [pretty tys, prettyDepth s] = undefined

tupleLitOf s@SS{depth_} tys | depth_ <1  = docError [
    "tupleLitOf depth_ <1",  pretty $ groom tys,  pretty s
    ]

tupleLitOf s@SS{..} types = do
    parts <- mapM mkParts types
    return $ ELit $ ETuple parts

    where
        mkParts ty  = do
            e <- exprOf s{depth_=depth_ - 1} ty
            return $ EExpr  e

relLitOf :: SpecState -> [Type] -> Gen Expr
relLitOf s tys | tracef "relLitOf" [pretty tys,  prettyDepth s] = undefined

relLitOf s@SS{depth_} tys | depth_ <2  = docError [
    "relLitOf depth_ <2",  pretty $ groom tys,  pretty s
    ]

relLitOf s@SS{..} types = do
    parts <-  vectorOf 3 $ mkParts types
    return $ ELit $ ERelation parts

    where
        mkParts tys = do
            (ELit lit) <- tupleLitOf s{depth_=depth_ - 1} tys
            return lit


parLitOf :: SpecState -> Type -> Gen Expr
parLitOf s@SS{..} innerType = do
    numElems <- choose (1, min 15 (2 * depth_) )
    parts <-  vectorOf numElems (mkPart innerType)
    return $ ELit $ EPartition parts

    where
        mkPart ty = do
            numElems <- choose (1, min 15 (2 * depth_) )
            vs <- vectorOf numElems ( exprOf s{depth_=depth_ - 1} ty)
            return $ map EExpr vs
