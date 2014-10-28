{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}

module TestGen.Arbitrary.Op where

import TestGen.Arbitrary.Helpers.Prelude

import TestGen.Arbitrary.Expr
import TestGen.Arbitrary.Type


type Bop = (Expr -> Expr -> BinOp)
type Uop = (Expr -> UniOp)

bop :: SpecState -> Bop ->  Gen Expr
bop s@SS{depth_} _ | depth_<1 =  docError
    [  "bop depth_ < 1", pretty s
    ]

bop s@SS{..} op =  do
    exprType <- atype s{depth_=depth_ - 1}

    e1 <- exprOf s{depth_=depth_ - 1} exprType
    e2 <- exprOf s{depth_=depth_ - 1} exprType

    return $ EBinOp $  op e1 e2


bopOf :: SpecState -> Bop -> Type -> Gen Expr
bopOf s@SS{depth_} op ty |  depth_<1 =  docError
    [  "bopOf depth_ < 1", pretty . groom $ ty, pretty s
    ]

bopOf s@SS{..} op exprType =  do
    e1 <- exprOf s{depth_=depth_ - 1} exprType
    e2 <- exprOf s{depth_=depth_ - 1} exprType

    return $ EBinOp $ op e1 e2

opOf :: SpecState -> Uop -> Type ->  Gen Expr
opOf s@SS{depth_} _ ty |  depth_<1 =  docError
    [  "opOf depth_ < 1", pretty . groom $ ty, pretty s
    ]

opOf s@SS{..} op exprType = do
    e1 <- exprOf s{depth_=depth_ - 1} exprType
    return $ EUniOp $ op e1


equivExpr :: SpecState -> Gen Expr
equivExpr s = oneof $ map (bop s) [ BEQ, BNEQ ]


arithmeticTypes :: SpecState -> Gen Type
arithmeticTypes _ = return TInt


arithmeticExpr :: SpecState -> Gen Expr
arithmeticExpr s = do
    kind <- arithmeticTypes s
    arithmeticExprOf s kind

arithmeticExprOf :: SpecState -> Type ->  Gen Expr
arithmeticExprOf s kind = do
    oneof $ map (flip (bopOf s) kind ) $ [BPlus, BMult, BDiv, BPow, BMod]


relationExpr :: SpecState -> Gen Expr
relationExpr s =  do
    oneof $ map (flip (bopOf s) TBool ) [BOr, BAnd, Bimply, Biff]

comparisonExpr :: SpecState -> Gen Expr
comparisonExpr s =  do
    oneof $ map (flip (bopOf s) TBool ) [BLT, BLTE, BGT, BGTE]


boolOpFor :: Type -> Gen (Expr -> Expr -> Expr)
boolOpFor TBool = do
    op <- elements [ BEQ, BNEQ, BOr, BAnd, Bimply, Biff ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor TInt = do
    op <- elements [ BEQ, BNEQ, BLT, BLTE, BGT, BGTE]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor (TSet _) =  do
    op <-  elements [ BEQ, BNEQ, BLT, Bsubset, BsubsetEq, Bsupset, BsupsetEq ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor (TMSet _) =  do
    op <-  elements [ BEQ, BNEQ, BLT, Bsubset, BsubsetEq, Bsupset, BsupsetEq ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor (TMatix _) = do
    op <- elements [BEQ, BNEQ ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor (TTuple _) = do
    op <- elements [BEQ, BNEQ ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor (TRel _) = do
    op <- elements [BEQ, BNEQ ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor (TFunc _ _) = do
    op <- elements [BEQ, BNEQ ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor (TPar _) = do
    op <- elements [BEQ, BNEQ ]
    return $ (\a b -> EBinOp $ op a  b )

boolOpFor  t = docError ["boolOpFor",pretty $  show t  ]
