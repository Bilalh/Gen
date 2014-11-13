{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, ScopedTypeVariables#-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE LambdaCase, MultiWayIf, TemplateHaskell #-}

module TestGen.Arbitrary.Expr where

import TestGen.Arbitrary.Helpers.Prelude

import TestGen.Arbitrary.Type
import TestGen.Arbitrary.Common

import {-# SOURCE #-} TestGen.Arbitrary.Literal
import {-# SOURCE #-} TestGen.Arbitrary.Op
import {-# SOURCE #-} TestGen.Arbitrary.FromNested

import qualified Data.Map as M


expr :: GG Expr
expr = do
    d <- gets depth_
    addLog "expr" ["depth_" <+> pretty d]

    if
        | d < 3     -> boolExpr
        | otherwise -> oneof2 [ boolExpr,quanExpr]


boolExpr :: GG Expr
boolExpr = do
    gets depth_ >>= \case
        0 -> oneof2 [ boolLit ]
        1 -> oneof2 [ boolLit, equivExpr, relationExpr ]
        _ -> oneof2 [ boolLit, equivExpr, relationExpr ]

quanExpr ::  GG Expr
quanExpr = oneof2 [ quanInExpr, quanOverExpr ]


quanInExpr :: GG Expr
quanInExpr  = withQuan $
    overs >>= \case
        Nothing -> boolExpr  -- Nothing to quantify over
        Just gen -> do

            over@(EVar overName) <- lift gen
            addLog "quanInExpr" ["over" <+> pretty over ]
            overType <- lookupType overName
            addLog "quanInExpr" ["overTy" <+> pretty overType ]

            let inType =  quanType_in overType
            inName <-  nextQuanVarName
            introduceVariable (inName, inType)

            addLog "quanInExpr" [ "in" <+> pretty inName
                                , "inTy" <+> pretty inType
                                ]

            -- FIXME Ensure with high prob that inName is actually used
            quanType <- elements2 [ ForAll, Exists ]
            let quanTop = EQuan quanType (BIn (EQVar inName) over)

            d <- gets depth_
            let typeDepth = depthOf inType
            let useGuardExpr = if
                    | typeDepth < fromIntegral d  ->
                        [withDepthDec $ boolExprUsingRef  inName]
                    | otherwise -> []

            addLog "quanInExpr" ["inDepth" <+> pretty typeDepth ]


            quanGuard <- oneof2 $ [ return EEmptyGuard ] ++ useGuardExpr
            quanBody <- withDepthDec boolExpr
            return $ quanTop quanGuard quanBody

    where
        overs =  varsOf'  (TSet TAny)

quanOverExpr :: GG Expr
quanOverExpr = withQuan $
    overs >>= \case
        Nothing -> boolExpr  -- Nothing to quantify over
        Just gen -> do
            addLog "quanOverExpr" []
            dom <- lift gen
            let overType = typeOfDom dom

            let innerType = overType
            inName <-  nextQuanVarName
            introduceVariable (inName, innerType)

            addLog "quanOverExpr" [ "in" <+> pretty inName
                                  , "inTy" <+> pretty innerType
                                  ]

            -- FIXME Ensure with high prob that inName is actually used
            quanType <- elements2 [ ForAll, Exists ]
            let quanTop = EQuan quanType (BOver (EQVar inName) (EDom dom))

            d <- gets depth_
            let typeDepth = depthOf innerType
            let useGuardExpr = if
                    | typeDepth < fromIntegral d  ->
                        [withDepthDec $ boolExprUsingRef  inName]
                    | otherwise -> []

            addLog "quanOverExpr" ["inDepth" <+> pretty typeDepth ]

            quanGuard <- oneof2 $ [ return EEmptyGuard ] ++ useGuardExpr
            quanBody <- withDepthDec boolExpr
            return $ quanTop quanGuard quanBody

    where
        overs = domOf [TSet TAny, TInt]


quanSum :: GG Expr
quanSum = withQuan $
    overs >>= \case
        Nothing  -> intLit
        Just gen -> do
            addLog "quanSum" []

            over@(EVar overName) <- lift gen
            overType <- lookupType overName

            let inType =  quanType_in overType
            inName <- nextQuanVarName
            introduceVariable  (inName, inType)

            addLog "quanSum" [ "in" <+> pretty inName
                                  , "inTy" <+> pretty inType
                                  ]

            let quanTop = EQuan Sum (BIn (EQVar inName) over)

            quanGuard <- oneof2 [
                return EEmptyGuard
                ]

            quanBody <-  withDepthDec $ exprOf TInt
            return $ quanTop quanGuard quanBody

    where
    overs = varsOf'  (TSet TInt)


-- assuming depth > 1 left
boolExprUsingRef :: Ref -> GG Expr
boolExprUsingRef ref = do
    d <- gets depth_
    addLog "boolExprUsingRef" ["depth_" <+> pretty d, "ref" <+> pretty ref]

    refType <- lookupType ref
    sidesType <- typeFromType refType

    addLog "boolExprUsingRef" ["refType" <+> pretty refType
                              , "sidesType" <+> pretty sidesType]


    other <- exprOf sidesType
    refExpr <- withDepth (min 2 d) $ exprFromToType ref refType sidesType

    onLeft :: Bool <- lift arbitrary
    op <- boolOpFor sidesType
    if onLeft then
        return $ op refExpr other
    else
        return $ op other refExpr

-- Types that can be reached from a type in n levels of nesting
exprFromToType :: Ref -> Type -> Type -> GG Expr
exprFromToType ref from to | from == to =  return $ EVar ref

exprFromToType ref (TSet _) TInt = return $ EUniOp $ UBar $ EVar ref




-- Return a expr of the specifed depth and type
exprOf :: Type -> GG Expr
exprOf ty = do
    nestedOfType <-  maybeToList <$> nestedVarsOf ty
    ofType <-  varsOf ty
    --FIXME varibles of TAny should be  restricted to the allowed depth
    let refs = (ofType ++ nestedOfType )

    d <- gets depth_
    addLog "exprOf" ["depth_" <+> pretty d, "ty"  <+> pretty ty ]

    -- Simple cases
    if
        | d < 0 -> ggError "exprOf depth_ <0" ["exprTy:" <+> pretty ty]
        | d == 0 && ty == TInt  -> oneof2 $ intLit : refs
        | d == 0 && ty == TBool -> oneof2 $ boolLit  : refs
        | d == 0 && ty == TAny  -> oneof2 $ [intLit, boolLit ] ++ refs
        | d == 0 -> ggError "exprOf depth_ <1" ["exprTy:" <+> pretty ty]
        | otherwise  ->  do
            newTy <- deAny ty
            addLog "exprOf" [nn "ty" ty, nn "newTy" newTy, nn "depth" d ]
            exprOf' d refs newTy


    where
    exprOf' :: Depth -> [GG Expr] -> Type -> GG Expr

    exprOf' _ ofType TBool = oneof2 $ ofType ++ [
          boolLit
        , equivExpr
        , relationExpr
        ]

    exprOf' 1 ofType TInt = oneof2 $ ofType ++ [
          intLit
        , arithmeticExprOf ty
        ]

    exprOf' _ ofType TInt = oneof2 $ ofType ++ [
          intLit
        , arithmeticExprOf ty
        , quanSum
        ]

    exprOf' _ ofType (TSet inner) = oneof2 $ ofType ++ [
          setLitOf inner
        ]

    exprOf' _ ofType (TMSet inner) = oneof2 $ ofType ++ [
          msetLitOf inner
        ]

    exprOf' _ ofType (TMatix inner) = frequency2 $ (map (\t -> (10,t)) ofType ) ++ [
          (1,matrixLitOf inner)
        ]

    exprOf' _ ofType (TFunc a b) = oneof2 $ ofType ++ [
          funcLitOf a b
        ]

    exprOf' d ofType (TRel tys) | d >= 2 = oneof2 $ ofType ++ [
          relLitOf tys
        ]

    exprOf' _ ofType (TPar inner) = oneof2 $ ofType ++ [
          parLitOf inner
        ]

    exprOf' _ ofType (TTuple tys) = oneof2 $ ofType ++ [
          tupleLitOf tys
        ]

    exprOf' d _ _  = ggError "exprOf not Matched other"
        ["exprDom:" <+> pretty ty, "d:" <+> pretty d ]


-- Remove one level of any
-- e.g for sets
deAny :: Type -> GG Type
deAny TAny = withSameDepth atype

deAny (TSet TAny)   = return TSet   <*> (withDepthDec atype)
deAny (TMSet TAny)  = return TMSet  <*> (withDepthDec atype)
deAny (TMatix TAny) = return TMatix <*> (withDepthDec atype)
deAny (TPar TAny)   = return TPar   <*> (withDepthDec atype)

deAny (TTuple ts) =  return TTuple <*> (mapM (withDepthDec . deAny) ts)
deAny (TRel   ts) =  do
    d <- gets depth_
    return TRel  <*> (mapM (withDepth (d - 2) . deAny) ts)

deAny (TFunc a b)   = return TFunc <*> (withDepthDec atype)
                                   <*> (withDepthDec atype)

deAny ty = return ty

-- at most one element
-- zero elements if beConstant_
varsOf ::Type -> GG [GG Expr]
varsOf ty = gets beConstant_ >>= \case
    False -> return []
    True  -> map lift . maybeToList <$> varsOf' ty


varsOf' :: Type -> GG (Maybe (Gen Expr))
varsOf' exprType = do
    SS{doms_,newVars_} <- get

    let newVars = map fst $ filter (typesUnify exprType . snd ) newVars_

    return $ toGenExpr EVar $ newVars ++ (
        map fst . M.toList  . M.filter
            (typesUnify exprType . typeOfDom . domOfFG ))  doms_



domOf ::  [Type] -> GG (Maybe (Gen Domain))
domOf exprTypes = do
    doms_ <- gets doms_
    return $ toGenExpr id  $ (map (domOfFG . snd) . M.toList  .
        M.filter (  (\t -> any (typesUnify t) exprTypes )  . typeOfDom . domOfFG ))
            doms_


toGenExpr ::  (a -> b) -> [a] -> Maybe (Gen b)
toGenExpr f vs =  case map f vs of
    [] -> Nothing
    xs -> Just $ elements xs
