{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Expr where

import Language.E
import AST.Imports
-- import TestGen.Arbitrary.Helpers
import TestGen.Arbitrary.Data
import TestGen.Arbitrary.Type
import TestGen.Arbitrary.Common

import {-# SOURCE #-} TestGen.Arbitrary.Literal
import {-# SOURCE #-} TestGen.Arbitrary.Op

import Test.QuickCheck
import Text.Groom(groom)
import qualified Data.Map as M


expr :: SpecState -> Gen Expr
expr s@SS{..} | depth_ < 3 = boolExpr s
expr s = oneof $
    [ boolExpr s, quanExpr s]

boolExpr :: SpecState -> Gen Expr
boolExpr s@SS{..} = oneof $ case depth_ of
     0 ->  [ boolLit s ]
     1 ->  [ boolLit s, equivExpr s, relationExpr s ]
     _ ->  [ boolLit s, equivExpr s, relationExpr s ]

quanExpr :: SpecState -> Gen Expr
quanExpr s = oneof $ [ quanInExpr s ]

quanInExpr :: SpecState -> Gen Expr
quanInExpr s =
    case overs of
        Nothing  -> boolExpr s  -- Nothing to quantify over
        Just gen-> do
            over@(EVar overName) <- gen
            let overType = lookupType s overName

            let inType =  quanType_in overType
            let (s', inName) = nextQuanVarName s
            let s'' = introduceVariable s' (inName, inType)

            -- FIXME Ensure with high prob that inName is actually used
            quanType <- elements [ ForAll, Exists ]
            let quanTop = EQuan quanType (BIn (EQVar inName) over)

            quanGuard <- oneof [
                return EEmptyGuard, exprUsingRef s''  inName
                ]
            quanBody <- boolExpr s''{depth_=depth_ s''  - 1}
            return $ quanTop quanGuard quanBody

    where
        overs =  varOf s (TSet TAny)

-- assuming depth > 1 left
exprUsingRef :: SpecState -> Ref -> Gen Expr
exprUsingRef s@SS{..} ref= do
    let refType = lookupType s ref
    sidesType <- typeFromType s refType

    other <- exprOf s sidesType
    refExpr <- exprFromToType s{depth_ = min 2 depth_} ref refType sidesType

    onLeft :: Bool <- arbitrary
    op <- boolOpFor sidesType
    if onLeft then
        return $ op refExpr other
    else
        return $ op other refExpr

exprFromToType :: SpecState -> Text -> Type -> Type -> Gen Expr
exprFromToType _ ref from to | from == to =  return $ EVar ref

exprFromToType s ref (TSet _) TInt = return $ EUniOp $ UBar $ EVar ref


-- Return a expr of the specifed depth and type
exprOf :: SpecState -> Type -> Gen Expr
exprOf s@SS{depth_=0,..} d@TBool = oneof $ ofType ++
    [
      boolLit s
    ]
    where ofType = maybeToList $ varOf s d

exprOf s@SS{..} d@TBool = oneof $ ofType ++
    [
      boolLit s
    , equivExpr s
    , relationExpr s
    ]
    where ofType = maybeToList $ varOf s d


exprOf s@SS{depth_=0,..} d@TInt = oneof $ ofType ++
    [
      intLit s
    ]
    where ofType = maybeToList $ varOf s d


exprOf s@SS{..} d@TInt = oneof $ ofType ++
    [
      intLit s
    , arithmeticExprOf s d
    ]
    where
    ofType = maybeToList $ varOf s d

exprOf s@SS{..} d@(TSet inner) | depth_ >=1 = oneof $ ofType ++
    [
       setLitOf s inner
    ]
    where ofType = maybeToList $ varOf s d

exprOf ss  exprDom = error . show . vcat $
    ["exprOfType not Matched", "exprDom:" <+> pretty exprDom, pretty . groom $ ss]


varOf :: SS -> Type -> Maybe (Gen Expr)
varOf SS{..} exprType = toGenExpr $ newVars ++  (map fst . M.toList  .
    M.filter (typesUnify exprType . typeOfDom . domOfFG ))  doms_

    where
    newVars :: [Text]
    newVars = map fst $ filter (typesUnify exprType . snd ) $ newVars_


toGenExpr :: [Ref] -> Maybe (Gen Expr)
toGenExpr doms =  case (map (EVar) $ doms) of
    [] -> Nothing
    xs -> Just $ elements xs