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
    -- [ boolExpr s]

boolExpr :: SpecState -> Gen Expr
boolExpr s@SS{..} = oneof $ case depth_ of
     0 ->  [ boolLit s ]
     1 ->  [ boolLit s, equivExpr s, relationExpr s ]
     _ ->  [ boolLit s, equivExpr s, relationExpr s ]


quanExpr :: SpecState -> Gen Expr
quanExpr s = oneof $ [ quanInExpr s, quanOverExpr s ]


quanInExpr :: SpecState -> Gen Expr
quanInExpr s | tracef "quanInExpr" [pretty s] = undefined
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
                return EEmptyGuard, boolExprUsingRef s''  inName
                ]
            quanBody <- boolExpr s''{depth_=depth_ s''  - 1}
            return $ quanTop quanGuard quanBody

    where
        overs =  varOf s (TSet TAny)


quanOverExpr :: SpecState -> Gen Expr
quanOverExpr s | tracef "quanInExpr" [pretty s] = undefined
quanOverExpr s =
    case overs of
        Nothing  -> boolExpr s  -- Nothing to quantify over

        Just gen -> do
            dom <- gen
            let overType = typeOfDom dom

            let innerType = overType
            let (s', inName) = nextQuanVarName s
            let s'' = introduceVariable s' (inName, innerType)

            -- FIXME Ensure with high prob that inName is actually used
            quanType <- elements [ ForAll, Exists ]
            let quanTop = EQuan quanType (BOver (EQVar inName) (EDom dom) )

            quanGuard <- oneof [
                return EEmptyGuard, boolExprUsingRef s''  inName
                ]
            quanBody <- boolExpr s''{depth_=depth_ s''  - 1}
            return $ quanTop quanGuard quanBody

    where
        overs =  domOf s [TSet TAny, TInt]


quanSum :: SpecState -> Gen Expr
quanSum s =
    case overs of
        Nothing -> intLit s
        Just gen -> do
            over@(EVar overName) <- gen
            let overType = lookupType s overName

            let inType =  quanType_in overType
            let (s', inName) = nextQuanVarName s
            let s'' = introduceVariable s' (inName, inType)

            let quanTop = EQuan Sum (BIn (EQVar inName) over)

            quanGuard <- oneof [
                return EEmptyGuard
                ]

            quanBody <-  exprOf s''{depth_=  depth_ s'' -1 } TInt
            return $ quanTop quanGuard quanBody

    where
    overs =  varOf s (TSet TInt)

-- assuming depth > 1 left
boolExprUsingRef :: SpecState -> Ref -> Gen Expr
boolExprUsingRef s@SS{..} ref | depth_ > 1= do
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

-- Types that can be reached from a type in n levels of nesting
exprFromToType :: SpecState -> Ref -> Type -> Type -> Gen Expr
exprFromToType _ ref from to | from == to =  return $ EVar ref

exprFromToType s ref (TSet _) TInt = return $ EUniOp $ UBar $ EVar ref

-- Return a expr of the specifed depth and type
exprOf :: SpecState -> Type -> Gen Expr
exprOf s ty | tracef "exprOf" [pretty ty, prettyDepth s] = undefined

exprOf s@SS{depth_} d  | depth_ < 0 =  docError $
    ["exprOf depth_ <0 ", "exprDom:" <+> pretty d, pretty . groom $ s]

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


exprOf s@SS{depth_=1,..} d@TInt = oneof $ ofType ++
    [
      intLit s
    , arithmeticExprOf s d
    ]
    where
    ofType = maybeToList $ varOf s d

exprOf s@SS{..} d@TInt = oneof $ ofType ++
    [
      intLit s
    , arithmeticExprOf s d
    , quanSum s
    ]
    where
    ofType = maybeToList $ varOf s d

exprOf s@SS{..} d@(TSet inner) | depth_ >=1 = oneof $ ofType ++
    [
       setLitOf s inner
    ]
    where ofType = maybeToList $ varOf s d

exprOf s@SS{..} d@(TMSet inner) | depth_ >=1 = oneof $ ofType ++
    [
       msetLitOf s inner
    ]
    where ofType = maybeToList $ varOf s d


exprOf s@SS{..} d@(TMatix inner) | depth_ >=1  = oneof $ ofType ++
    [
       matrixLitOf s inner
    ]
    where ofType = maybeToList $ varOf s d

exprOf s@SS{..} d@(TFunc a b) | depth_ >=1  = oneof $ ofType ++
    [
       funcLitOf s a b
    ]
    where ofType = maybeToList $ varOf s d

exprOf s@SS{..} d@(TRel tys)  | depth_ >=2  = oneof $ ofType ++
    [
       relLitOf s tys
    ]
    where ofType = maybeToList $ varOf s d

exprOf s@SS{..} d@(TPar inner)  | depth_ >=1  = oneof $ ofType ++
    [
       parLitOf s inner
    ]
    where ofType = maybeToList $ varOf s d

exprOf s@SS{..} d@(TTuple tys)  | depth_ >=1  = oneof $ ofType ++
    [
       tupleLitOf s tys
    ]
    where ofType = maybeToList $ varOf s d


exprOf s@SS{..} d@(TUnamed _ )  =  docError $
    ["exprOf not Matched", "exprDom:" <+> pretty d, pretty . groom $ s]

exprOf s@SS{..} d@(TEnum _ )  =  docError $
    ["exprOf not Matched", "exprDom:" <+> pretty d, pretty . groom $ s]


exprOf s@SS{..} d@(TAny  )  =  docError $
    ["exprOf not Matched", "exprDom:" <+> pretty d, pretty . groom $ s]

exprOf s d  =  docError $
    ["exprOf not Matched other", "exprDom:" <+> pretty d, pretty . groom $ s]


varOf :: SpecState -> Type -> Maybe (Gen Expr)
varOf SS{..} exprType = toGenExpr EVar $ newVars ++ (
    map fst . M.toList  . M.filter
        (typesUnify exprType . typeOfDom . domOfFG ))  doms_

    where
    newVars :: [Text]
    newVars = map fst $ filter (typesUnify exprType . snd ) $ newVars_

domOf :: SpecState -> [Type] -> Maybe (Gen Domain)
domOf SS{..} exprTypes = toGenExpr id  $ (map (domOfFG . snd) . M.toList  .
    M.filter (  (\t -> any (typesUnify t) exprTypes )  . typeOfDom . domOfFG ))
        doms_



toGenExpr ::  (a -> b) -> [a] -> Maybe (Gen b)
toGenExpr f vs =  case (map f vs) of
    [] -> Nothing
    xs -> Just $ elements xs
