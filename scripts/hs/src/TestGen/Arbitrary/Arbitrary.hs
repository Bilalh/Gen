{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module TestGen.Arbitrary.Arbitrary where

import AST.Imports
import TestGen.Arbitrary.Helpers

import Language.E hiding(trace)
-- import Debug.Trace(trace)

import Test.QuickCheck
-- import Control.Monad(liftM2)

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Map(Map)
import Control.Monad.Trans.State.Strict(State)

type Depth = Int

type SpecM a = State SpecState a
type GenM  a = State SpecState (Gen a)

data SpecState = SpecState
    {
      depth_   :: Depth       --  how many levels to genrate
    , doms_    :: Map Text FG --  Domains
    , nextNum_ :: Int          -- Number to name next var
    }





instance Arbitrary SpecE where
    arbitrary = sized spec1

spec1 :: Depth -> Gen SpecE
spec1 depth = do
    doms <- listOfB 1 10 (dom depth)
    let withNames =  zipWith (\d i -> (name i , Find d)) doms [1 :: Int ..]
    let mappings  = M.fromList withNames
    undefined

    let state = SpecState{depth_=depth, doms_=mappings, nextNum_ = length doms}

    let exprGen =  runIdentity $  evalStateT boolExpr state
    exprs <- listOfB 0 15 ( exprGen )

    return $ SpecE mappings exprs

    where name i =  T.pack $  "var" ++  (show  i)


spec :: Depth -> Gen SpecE
spec depth = do
    doms <- listOfB 1 10 (dom depth)
    let withNames =  zipWith (\d i -> (name i , Find d)) doms [1 :: Int ..]
    let mappings = M.fromList withNames

    exprs <- listOfB 0 15 (expr depth mappings)



    return $ SpecE mappings exprs

    where name i =  T.pack $  "var" ++  (show  i)

---- Domains

dom :: Depth -> Gen (Domain)
dom 0 = oneof
    [
      intDom 0
    , return DBool
    ]

dom n = oneof
    [
      intDom (n - 1)
    , return DBool
    , setDom (n - 1)
    ]

intDom :: Depth -> Gen Domain
intDom _ = return DInt `ap` (listOfB 1 4 arbitrary)

setDom :: Depth -> Gen Domain
setDom depth = do
    inner <- dom depth
    return $ dset{inner}

---- lits

boolLit1 :: SpecM (Gen Expr)
boolLit1 =
    let b = do
            bb <- arbitrary
            return $ ELit (EB bb)
    in return  b

    -- return $ (liftM ELit (liftM EB arbitrary))
    -- return $  (do {  dd <- (liftM EB arbitrary); return $ ELit dd  }  )

boolLit :: Gen Expr
boolLit = do
    b <- arbitrary
    return  (ELit (EB b))

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

boolExpr :: GenM Expr
boolExpr =do
    aa <- sequence [boolLit1]

    return $ oneof aa

-- An expression that results in a boolean
expr :: Depth -> Doms ->  Gen Expr
expr depth doms = oneof $ case depth of
        0 -> c0; 1 -> c1; _ -> c2

    where
    c0 =
        [
          boolLit
        ]
    c1 =  c0 ++
        [
          bop depth doms BEQ
        , bop depth doms BNEQ
        ]
    c2 = c1 ++
        [
          quanExpr depth doms
        ]


quanExpr :: Depth -> Doms -> Gen Expr
quanExpr depth doms = do

    bs <- expr (depth - 1) doms
    let a = EQuan ForAll (BIn (EQVar "x") (EVar "a")) EEmptyGuard
                bs
    return $ a

----OPS

type Bop = (Expr -> Expr -> BinOp)

bop :: Depth -> Doms -> Bop ->  Gen Expr
bop depth doms op =  do
    -- TODO we what domain without attributes, for type checking
    exprDom <- dom (depth - 1)

    e1 <- exprOf (depth - 1) doms exprDom
    e2 <- exprOf (depth - 1) doms exprDom

    return $ EBinOp $  op e1 e2

bopOf depth doms exprDom op =  do

    e1 <- exprOf (depth - 1) doms exprDom
    e2 <- exprOf (depth - 1) doms exprDom

    return $ EBinOp $ op e1 e2


opOf depth doms exprDom op =  do
    e1 <- exprOf (depth - 1) doms exprDom
    return $ EUniOp $ op e1

-- OfType

-- pick a type,   choose from all the way to genrate that type, i.e lit.
exprOf :: Depth -> Doms -> Domain -> Gen Expr
exprOf 0 doms d@DBool = oneof $ ofType ++
    [
      do { b <- arbitrary; return (ELit (EB b) ) }
    ]
    where ofType = maybeToList $ varOf doms d


exprOf depth doms d@DBool = oneof $ ofType ++
    [
      do { b <- arbitrary; return (ELit (EB b) ) } -- Literal
    , bop depth doms BEQ
    ]
    where ofType = maybeToList $ varOf doms d


exprOf 0 doms d@DInt{..} = oneof $ ofType ++
    [
       do { i <- choose ((-10),10 :: Integer); return (ELit (EI i) ) }
    ]
    where ofType = maybeToList $ varOf doms d

exprOf 1 doms d@DInt{..} = oneof $ ofType ++
    [
       do { i <- choose ((-10),10 :: Integer); return (ELit (EI i) ) }
    , bopOf 1 doms d (BPlus)
    ]
    where ofType = maybeToList $ varOf doms d

exprOf depth doms d@DInt{..} = oneof $ ofType ++
    [
      do { i <- choose ((-10),10 :: Integer); return (ELit (EI i) ) }
    , bopOf depth doms d ( BPlus)
    , bar
    ]
    where
    ofType = maybeToList $ varOf doms d
    bar = do
        edom <- setDom (depth - 1)
        opOf depth doms edom ( UBar)

exprOf 0 doms d@DSet{..} = error "set depth 0"

exprOf 1 doms d@DSet{..} = oneof $ ofType ++
    [
       do { i <- choose ((-10),10 :: Integer); return (ELit (ESet [EI i] ) ) }
    ]
    where ofType = maybeToList $ varOf doms d


exprOf depth doms d@DSet{..} = oneof $ ofType ++
    [
        do { i <- choose ((-10),10 :: Integer); return (ELit (ESet [EI i]) ) }
    ]
    where
    ofType = maybeToList $ varOf doms d


    useInner = do
        innerExpr <- exprOf (depth - 1) doms inner
        -- expr in lit
        undefined


exprOf depth doms dom = error . show . vcat $
    ["exprOfType not Matched", pretty depth, pretty dom,pretty doms]


varOf :: Doms -> Domain -> Maybe (Gen Expr)
varOf doms dom = toGenExpr $  M.filter (typesUnify dom . domOfFG) doms


toGenExpr :: Doms -> Maybe (Gen Expr)
toGenExpr doms =  case (map (EVar . fst) . M.toList $ doms) of
    [] -> Nothing
    xs -> Just $ elements xs
