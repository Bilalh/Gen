{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module TestGen.QC.ArbitrarySpec where

import Language.E hiding(trace)

import AST.Constraint
import AST.Domain
import AST.Literal
import AST.SpecE
-- import AST.ToEssence

import Test.QuickCheck
import Control.Monad(liftM2)
import qualified Data.Text as T

import qualified Data.Map as M

-- import Language.E hiding(trace)
-- import Debug.Trace(trace)

instance Arbitrary SpecE where
    arbitrary = sized arbitrarySpec


arbitrarySpec :: Int -> Gen SpecE
arbitrarySpec depth = do
    doms <- listOf1 (arbitraryDom depth)
    let withNames =  zipWith (\d i -> (name i , Find d)) doms [1 :: Int ..]
    let mappings = M.fromList withNames

    exprs <- listOf (arbitraryExpr depth mappings)

    return $ SpecE mappings  (ELit (EB True) :  exprs)

    where name i =  T.pack $  "var" ++  (show  i)


arbitraryDom :: Int -> Gen (Domain)
arbitraryDom _ =oneof
    [
        -- return DInt `ap` arbitrary
       return DBool
    ]


arbitraryExpr :: Int -> Doms ->  Gen Expr
arbitraryExpr 0 _ =do
    b <- arbitrary
    return (ELit (EB b) )


arbitraryExpr depth doms =
    oneof
        [
             do { b <- arbitrary; return (ELit (EB b) ) } -- Boolean Literal
            ,arbitraryBop depth doms EEQ
        ]

type Bop = (Expr -> Expr -> Expr)

arbitraryBop :: Int -> Doms -> Bop  -> Gen Expr
arbitraryBop depth doms op =  do
    exprDom <- arbitraryDom (depth - 1)

    e1 <- exprOfType (depth - 1) doms exprDom
    e2 <- exprOfType (depth - 1) doms exprDom

    return $ op e1 e2

    -- let res = op e1 e2
        -- typee = typeOfC (toEssence res)
    -- return $ (trace . show . pretty $ typee ) res

-- pick a type,   choose from all the way to genrate that type.

exprOfType :: Int -> Doms -> Domain -> Gen Expr
exprOfType 0 doms DBool = oneof
    [
      do { b <- arbitrary; return (ELit (EB b) ) } -- Literal
    ]

exprOfType depth doms DBool = oneof
    [
      do { b <- arbitrary; return (ELit (EB b) ) } -- Literal
    , arbitraryBop (depth - 1) doms EEQ
    ]


exprOfType depth doms dom = error . show . vcat $ [pretty depth, pretty dom]


typeOfC :: E -> E
typeOfC e  =
    let (mresult, _logs) = runCompESingle "typeOf" helper
    in case mresult of
        Right ss ->  ss
        Left d     -> error . show .  vcat $ ["typeOf", d, pretty _logs]

    where
        helper = do
            typeOf e
