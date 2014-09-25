{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module TestGen.QC.ArbitrarySpec where

import Language.E hiding(trace)

import AST.Expr
import AST.Domain
import AST.Literal
import AST.SpecE
import AST.ToEssence

import Test.QuickCheck
-- import Control.Monad(liftM2)
import qualified Data.Text as T

import qualified Data.Map as M

-- import Language.E hiding(trace)
-- import Debug.Trace(trace)

instance Arbitrary SpecE where
    arbitrary = sized arbitrarySpec


arbitrarySpec :: Int -> Gen SpecE
arbitrarySpec depth = do
    doms <- listOfB 1 10 (arbitraryDom depth)
    let withNames =  zipWith (\d i -> (name i , Find d)) doms [1 :: Int ..]
    let mappings = M.fromList withNames

    exprs <- listOfB 0 15 (arbitraryExpr depth mappings)

    return $ SpecE mappings exprs

    where name i =  T.pack $  "var" ++  (show  i)


arbitraryDom :: Int -> Gen (Domain)
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

arbitraryIntDomain :: Int -> Gen Domain
arbitraryIntDomain _ = return DInt `ap` (listOfB 1 4 arbitrary)

arbitrarySetDomain :: Int -> Gen Domain
arbitrarySetDomain depth = do
    inner <- arbitraryDom depth
    return $ dset{inner}


-- | Generates a random length between the specifed bounds.
--   The maximum length depends on the size parameter.
listOfB :: Int -> Int -> Gen a -> Gen [a]
listOfB l u gen = sized $ \n -> do
    k <- choose ( 0 `max` l, u `min` n)
    vectorOf k gen


instance Arbitrary (Range Integer) where
    arbitrary = oneof
        [
          liftM RSingle (choose ((-5),5 :: Integer))
        , arbitraryFromTo
        ]

        where
        arbitraryFromTo :: Gen (Range Integer)
        arbitraryFromTo = do
            do
                a <- choose ((-10),10 :: Integer)
                b <- choose (a,10)
                return $ RFromTo a b

    shrink x = genericShrink x


arbitraryExpr :: Int -> Doms ->  Gen Expr
arbitraryExpr 0 _ =do
    b <- arbitrary
    return (ELit (EB b) )

arbitraryExpr depth doms = oneof
        [
          do { b <- arbitrary; return (ELit (EB b) ) }
        , arbitraryBop depth doms EEQ
        ]


type Bop = (Expr -> Expr -> Expr)

arbitraryBop :: Int -> Doms -> Bop  -> Gen Expr
arbitraryBop depth doms op =  do
    -- TODO we what domain without attributes, for type checking
    exprDom <- arbitraryDom (depth - 1)

    e1 <- exprOfType (depth - 1) doms exprDom
    e2 <- exprOfType (depth - 1) doms exprDom

    return $ op e1 e2


-- pick a type,   choose from all the way to genrate that type.
exprOfType :: Int -> Doms -> Domain -> Gen Expr
exprOfType 0 doms d@DBool = oneof $ ofType ++
    [
      do { b <- arbitrary; return (ELit (EB b) ) }
    ]
    where ofType = maybeToList $ varOfType doms d


exprOfType depth doms d@DBool = oneof $ ofType ++
    [
      do { b <- arbitrary; return (ELit (EB b) ) } -- Literal
    , arbitraryBop depth doms EEQ
    ]
    where ofType = maybeToList $ varOfType doms d


exprOfType 0 doms d@DInt{..} = oneof $ ofType ++
    [
       do { i <- choose ((-10),10 :: Integer); return (ELit (EI i) ) }
    ]
    where ofType = maybeToList $ varOfType doms d

exprOfType depth doms d@DInt{..} = oneof $ ofType ++
    [
       do { i <- choose ((-10),10 :: Integer); return (ELit (EI i) ) }
    , arbitraryBop depth doms EEQ
    ]
    where ofType = maybeToList $ varOfType doms d


exprOfType depth doms dom = error . show . vcat $
    ["exprOfType not Matched", pretty depth, pretty dom,pretty doms]


varOfType :: Doms -> Domain -> Maybe (Gen Expr)
varOfType doms dom = toGenExpr $  M.filter (typesUnify dom . domOfFG) doms


-- TODO could be a lot more efficient
typesUnify  :: Domain -> Domain -> Bool
typesUnify a b = typeUnify (toEssence a) (toEssence b)


toGenExpr :: Doms -> Maybe (Gen Expr)
toGenExpr doms =  case (map (EVar . fst) . M.toList $ doms) of
    [] -> Nothing
    xs -> Just $ elements xs


typeOfC :: E -> E
typeOfC e  =
    let (mresult, _logs) = runCompESingle "typeOf" helper
    in case mresult of
        Right ss ->  ss
        Left d     -> error . show .  vcat $ ["typeOf", d, pretty _logs]

    where
        helper = do
            typeOf e

-- _sample :: Int -> IO ()
_sample e  = do
    a <- sample' e
    mapM_  (putStrLn  . show . pretty) a
