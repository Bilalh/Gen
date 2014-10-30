{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module TestGen.Arbitrary.Op where

import TestGen.Arbitrary.Helpers.Prelude

import TestGen.Arbitrary.Expr
import TestGen.Arbitrary.Type


type Bop = (Expr -> Expr -> BinOp)
type Uop = (Expr -> UniOp)

bop :: Bop ->  GG Expr
bop op = do
    s@SS{depth_} <- get
    if
        | depth_ < 1 -> ggError "bop depth_ < 1" [ ]
        | otherwise -> do
            let newDepth = depth_ - 1
            exprType <-  withDepthDec atype
            e1 <- withDepth newDepth (exprOf exprType)
            e2 <- withDepth newDepth (exprOf exprType)

            return $ EBinOp $  op e1 e2


bopOf :: Bop -> Type -> GG Expr
bopOf op exprType = do
    s@SS{depth_} <- get
    if
        | depth_ < 1 -> ggError "bopOf depth_ < 1" [pretty exprType]

        | otherwise -> do
            e1 <- withDepthDec (exprOf exprType)
            e2 <- withDepthDec (exprOf exprType)

            return $ EBinOp $ op e1 e2


opOf :: Uop -> Type ->  GG Expr
opOf op exprType =  do
    depth_ <- gets depth_
    if
        | depth_ < 1 -> ggError "opOf depth_ < 1" [ pretty . groom $ exprType ]
        | otherwise -> do
            e1 <- withDepthDec (exprOf exprType)
            return $ EUniOp $ op e1


equivExpr :: GG Expr
equivExpr = oneof2 $ map (bop) [ BEQ, BNEQ ]


arithmeticTypes :: GG Type
arithmeticTypes  = return TInt


arithmeticExpr :: GG Expr
arithmeticExpr = do
    kind <- arithmeticTypes
    arithmeticExprOf kind

arithmeticExprOf :: Type ->  GG Expr
arithmeticExprOf kind = do
    oneof2 $ map (flip (bopOf) kind ) $ [BPlus, BMult, BDiv, BPow, BMod]


relationExpr :: GG Expr
relationExpr =  do
    oneof2 $ map (flip bopOf TBool ) [BOr, BAnd, Bimply, Biff]

comparisonExpr :: GG Expr
comparisonExpr =  do
    oneof2 $ map (flip bopOf TBool ) [BLT, BLTE, BGT, BGTE]


boolOpFor :: Type -> GG (Expr -> Expr -> Expr)
boolOpFor TBool = do
    op <- elements2 [ BEQ, BNEQ, BOr, BAnd, Bimply, Biff ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor TInt = do
    op <- elements2 [ BEQ, BNEQ, BLT, BLTE, BGT, BGTE]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor (TSet _) =  do
    op <-  elements2 [ BEQ, BNEQ, BLT, Bsubset, BsubsetEq, Bsupset, BsupsetEq ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor (TMSet _) =  do
    op <-  elements2 [ BEQ, BNEQ, BLT, Bsubset, BsubsetEq, Bsupset, BsupsetEq ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor (TMatix _) = do
    op <- elements2 [BEQ, BNEQ ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor (TTuple _) = do
    op <- elements2 [BEQ, BNEQ ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor (TRel _) = do
    op <- elements2 [BEQ, BNEQ ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor (TFunc _ _) = do
    op <- elements2 [BEQ, BNEQ ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor (TPar _) = do
    op <- elements2 [BEQ, BNEQ ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor  t = ggError "boolOpFor" [pretty t]
