{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module TestGen.QC.ArbitrarySpec where

import AST.Imports
import TestGen.QC.Helpers

import Language.E hiding(trace)
-- import Debug.Trace(trace)

import Test.QuickCheck
-- import Control.Monad(liftM2)

import qualified Data.Text as T
import qualified Data.Map as M

type Depth = Int


instance Arbitrary SpecE where
    arbitrary = sized arbitrarySpec

arbitrarySpec :: Depth -> Gen SpecE
arbitrarySpec depth = do
    doms <- listOfB 1 10 (arbitraryDom depth)
    let withNames =  zipWith (\d i -> (name i , Find d)) doms [1 :: Int ..]
    let mappings = M.fromList withNames

    exprs <- listOfB 0 15 (arbitraryExpr depth mappings)

    return $ SpecE mappings exprs

    where name i =  T.pack $  "var" ++  (show  i)

---- Domains

arbitraryDom :: Depth -> Gen (Domain)
arbitraryDom 0 = oneof
    [
      arbitraryIntDomain 0
    , return DBool
    ]

arbitraryDom n = oneof
    [
      arbitraryIntDomain (n - 1)
    , return DBool
    , arbitrarySetDomain (n - 1)
    ]

arbitraryIntDomain :: Depth -> Gen Domain
arbitraryIntDomain _ = return DInt `ap` (listOfB 1 4 arbitrary)

arbitrarySetDomain :: Depth -> Gen Domain
arbitrarySetDomain depth = do
    inner <- arbitraryDom depth
    return $ dset{inner}

---- Ranges


instance Arbitrary (Range Expr) where
    arbitrary = oneof
        [
        --   liftM RSingle (choose ((-5),5 :: Integer))
         arbitraryFromTo
        ]

        where
        arbitraryFromTo :: Gen (Range Expr)
        arbitraryFromTo = do
            do
                a <- choose ((-10),10 :: Integer)
                b <- choose (a,10)
                return $ RFromTo (ELit . EI $ a) (ELit . EI $  b)

    -- shrink x = genericShrink x

---EXPR

-- An expression that results in a boolean
arbitraryExpr :: Depth -> Doms ->  Gen Expr
arbitraryExpr depth doms = oneof $ case depth of
        0 -> c0; 1 -> c1; _ -> c2

    where
    c0 =
        [
          do { b <- arbitrary; return (ELit (EB b) ) }
        ]
    c1 =  c0 ++
        [
          arbitraryBop depth doms BEQ
        , arbitraryBop depth doms BNEQ
        ]
    c2 = c1 ++
        [
          arbitraryQuan depth doms
        ]


arbitraryQuan :: Depth -> Doms -> Gen Expr
arbitraryQuan depth doms = do

    bs <- arbitraryExpr (depth - 1) doms
    let a = EQuan ForAll (BIn (EQVar "x") (EVar "a")) EEmptyGuard
                bs
    return $ a

----OPS

type Bop = (Expr -> Expr -> BinOp)

arbitraryBop :: Depth -> Doms -> Bop ->  Gen Expr
arbitraryBop depth doms op =  do
    -- TODO we what domain without attributes, for type checking
    exprDom <- arbitraryDom (depth - 1)

    e1 <- exprOfType (depth - 1) doms exprDom
    e2 <- exprOfType (depth - 1) doms exprDom

    return $ EBinOp $  op e1 e2

ofTypeBop depth doms exprDom op =  do

    e1 <- exprOfType (depth - 1) doms exprDom
    e2 <- exprOfType (depth - 1) doms exprDom

    return $ EBinOp $ op e1 e2


ofTypeOp depth doms exprDom op =  do
    e1 <- exprOfType (depth - 1) doms exprDom
    return $ EUniOp $ op e1

-- OfType

-- pick a type,   choose from all the way to genrate that type, i.e lit.
exprOfType :: Depth -> Doms -> Domain -> Gen Expr
exprOfType 0 doms d@DBool = oneof $ ofType ++
    [
      do { b <- arbitrary; return (ELit (EB b) ) }
    ]
    where ofType = maybeToList $ varOfType doms d


exprOfType depth doms d@DBool = oneof $ ofType ++
    [
      do { b <- arbitrary; return (ELit (EB b) ) } -- Literal
    , arbitraryBop depth doms BEQ
    ]
    where ofType = maybeToList $ varOfType doms d


exprOfType 0 doms d@DInt{..} = oneof $ ofType ++
    [
       do { i <- choose ((-10),10 :: Integer); return (ELit (EI i) ) }
    ]
    where ofType = maybeToList $ varOfType doms d

exprOfType 1 doms d@DInt{..} = oneof $ ofType ++
    [
       do { i <- choose ((-10),10 :: Integer); return (ELit (EI i) ) }
    , ofTypeBop 1 doms d (BPlus)
    ]
    where ofType = maybeToList $ varOfType doms d

exprOfType depth doms d@DInt{..} = oneof $ ofType ++
    [
      do { i <- choose ((-10),10 :: Integer); return (ELit (EI i) ) }
    , ofTypeBop depth doms d ( BPlus)
    , bar
    ]
    where
    ofType = maybeToList $ varOfType doms d
    bar = do
        edom <- arbitrarySetDomain (depth - 1)
        ofTypeOp depth doms edom ( UBar)

exprOfType 0 doms d@DSet{..} = error "set depth 0"

exprOfType 1 doms d@DSet{..} = oneof $ ofType ++
    [
       do { i <- choose ((-10),10 :: Integer); return (ELit (ESet [EI i] ) ) }
    ]
    where ofType = maybeToList $ varOfType doms d


exprOfType depth doms d@DSet{..} = oneof $ ofType ++
    [
        do { i <- choose ((-10),10 :: Integer); return (ELit (ESet [EI i]) ) }
    ]
    where
    ofType = maybeToList $ varOfType doms d


    useInner = do
        innerExpr <- exprOfType (depth - 1) doms inner
        -- expr in lit
        undefined


exprOfType depth doms dom = error . show . vcat $
    ["exprOfType not Matched", pretty depth, pretty dom,pretty doms]


varOfType :: Doms -> Domain -> Maybe (Gen Expr)
varOfType doms dom = toGenExpr $  M.filter (typesUnify dom . domOfFG) doms


toGenExpr :: Doms -> Maybe (Gen Expr)
toGenExpr doms =  case (map (EVar . fst) . M.toList $ doms) of
    [] -> Nothing
    xs -> Just $ elements xs
