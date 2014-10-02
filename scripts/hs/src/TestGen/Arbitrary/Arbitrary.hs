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
import Text.Groom(groom)

type Depth = Int
type GenM  a = State SpecState (Gen a)

data SS = SS
    {
      depth_   :: Depth       --  how many levels to genrate
    , doms_    :: Map Text FG --  Domains
    , nextNum_ :: Int          -- Number to name next var
    } deriving Show
type SpecState=SS

instance Arbitrary SpecE where
    arbitrary = sized spec

spec :: Depth -> Gen SpecE
spec depth = do
    doms <- listOfB 1 10 (dom depth)
    let withNames =  zipWith (\d i -> (name i , Find d)) doms [1 :: Int ..]
    let mappings  = M.fromList withNames

    let state = SS{depth_=depth, doms_=mappings, nextNum_ = length doms}

    exprs <- listOfB 0 15 ( boolExpr state)

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
    --FIXME depth?
    innerDom <- dom depth_
    exprs <- listOfB 0 15 ( exprOf s innerDom)
    return $ ELit $ ESet $ map EExpr $ exprs


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


--- Exps

boolExpr :: SpecState -> Gen Expr
boolExpr s@SS{..} = oneof $ case depth_ of
     0 ->  [ boolLit s ]
     1 ->  [ boolLit s, bop s BEQ ]
     _ ->  [ boolLit s, bop s BEQ ] -- quan ...

-- quanExpr :: Depth -> Doms -> Gen Expr
-- quanExpr depth doms = do
--
--     bs <- expr (depth - 1) doms
--     let a = EQuan ForAll (BIn (EQVar "x") (EVar "a")) EEmptyGuard
--                 bs
--     return $ a

----OPS

type Bop = (Expr -> Expr -> BinOp)

bop :: SS -> Bop ->  Gen Expr
bop s@SS{..} op =  do
    -- TODO we what domain without attributes, for type checking
    exprDom <- dom (depth_ -1)

    e1 <- exprOf s{depth_=depth_ - 1} exprDom
    e2 <- exprOf s{depth_=depth_ - 1} exprDom

    return $ EBinOp $  op e1 e2


bopOf :: SS -> Bop -> Domain -> Gen Expr
bopOf s@SS{..} op exprDom =  do
    e1 <- exprOf s{depth_=depth_ - 1} exprDom
    e2 <- exprOf s{depth_=depth_ - 1} exprDom

    return $ EBinOp $ op e1 e2

opOf :: SS -> (Expr -> UniOp) -> Domain ->  Gen Expr
opOf s@SS{..} op exprDom = do
    e1 <- exprOf s{depth_=depth_ - 1} exprDom
    return $ EUniOp $ op e1

bar :: SS -> Gen Expr
bar s@SS{..} = do
    edom <- undefined :: Gen Domain
    opOf s UBar edom

-- pick a type,   choose from all the way to genrate that type, i.e lit.
exprOf :: SS -> Domain -> Gen Expr
exprOf s@SS{depth_=0,..} d@DBool = oneof $ ofType ++
    [
      boolLit s
    ]
    where ofType = maybeToList $ varOf s d

exprOf s@SS{..} d@DBool = oneof $ ofType ++
    [
      boolLit s
    , bop s BEQ
    ]
    where ofType = maybeToList $ varOf s d


exprOf s@SS{depth_=0,..} d@DInt{} = oneof $ ofType ++
    [
      intLit s
    ]
    where ofType = maybeToList $ varOf s d

exprOf s@SS{depth_=1,..} d@DInt{} = oneof $ ofType ++
    [
      intLit s
    , bopOf s BPlus d
    ]
    where ofType = maybeToList $ varOf s d

exprOf s@SS{..} d@DInt{} = oneof $ ofType ++
    [
      intLit s
    , bopOf s BPlus d
    ]
    where
    ofType = maybeToList $ varOf s d

exprOf s@SS{depth_=0, ..} exprDom = error . show . vcat $
    ["setSize 0", pretty exprDom, pretty . groom $ s]

exprOf s@SS{..} d@DSet{..} = oneof $ ofType ++
    [
       setLit s
    ]
    where ofType = maybeToList $ varOf s d

exprOf ss  exprDom = error . show . vcat $
    ["exprOfType not Matched", pretty exprDom, pretty . groom $ ss]


varOf :: SS -> Domain -> Maybe (Gen Expr)
varOf SS{..} exprDom = toGenExpr $  M.filter (typesUnify exprDom . domOfFG) doms_


toGenExpr :: Doms -> Maybe (Gen Expr)
toGenExpr doms =  case (map (EVar . fst) . M.toList $ doms) of
    [] -> Nothing
    xs -> Just $ elements xs




---Old

boolExpr2 :: GenM Expr
boolExpr2 =do
    aa <- sequence [boolLit2]

    return $ oneof aa



boolLit2 :: GenM Expr
boolLit2 =
    let b = do
            bb <- arbitrary
            return $ ELit (EB bb)
    in return  b

    -- return $ (liftM ELit (liftM EB arbitrary))
    -- return $  (do {  dd <- (liftM EB arbitrary); return $ ELit dd  }  )
