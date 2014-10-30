{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, ScopedTypeVariables#-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Expr where

import TestGen.Arbitrary.Helpers.Prelude

import TestGen.Arbitrary.Type
import TestGen.Arbitrary.Common

import {-# SOURCE #-} TestGen.Arbitrary.Literal
import {-# SOURCE #-} TestGen.Arbitrary.Op

import qualified Data.Map as M


expr :: GG Expr
expr = do
    gets depth_ >>= \d -> if
        | d < 3     -> boolExpr
        | otherwise -> oneof2 [ boolExpr,quanExpr]


boolExpr :: GG Expr
boolExpr = do
    gets depth_ >>= \case
        0 -> oneof2 [ boolLit ]
        1 -> oneof2 [ boolLit, equivExpr, relationExpr ]
        _ -> oneof2 [ boolLit, equivExpr, relationExpr ]

quanExpr :: GG Expr
quanExpr = error "quanExprquanExpr"

-- quanExpr  GG Expr
-- quanExpr s = oneof $ [ quanInExpr s, quanOverExpr s ]
--
--
-- quanInExpr  GG Expr
-- quanInExpr s | tracef "quanInExpr" [pretty s] = undefined
-- quanInExpr s =
--     case overs of
--         Nothing  -> boolExpr s  -- Nothing to quantify over
--         Just gen-> do
--             over@(EVar overName) <- gen
--             let overType = lookupType s overName
--
--             let inType =  quanType_in overType
--             let (s', inName) = nextQuanVarName s
--             let s'' = introduceVariable s' (inName, inType)
--
--             -- FIXME Ensure with high prob that inName is actually used
--             quanType <- elements [ ForAll, Exists ]
--             let quanTop = EQuan quanType (BIn (EQVar inName) over)
--
--             quanGuard <- oneof [
--                 return EEmptyGuard, boolExprUsingRef s''  inName
--                 ]
--             quanBody <- boolExpr s''{depth_=depth_ s''  - 1}
--             return $ quanTop quanGuard quanBody
--
--     where
--         overs =  varOf s (TSet TAny)
--
--
-- quanOverExpr  GG Expr
-- quanOverExpr s | tracef "quanInExpr" [pretty s] = undefined
-- quanOverExpr s =
--     case overs of
--         Nothing  -> boolExpr s  -- Nothing to quantify over
--
--         Just gen -> do
--             dom <- gen
--             let overType = typeOfDom dom
--
--             let innerType = overType
--             let (s', inName) = nextQuanVarName s
--             let s'' = introduceVariable s' (inName, innerType)
--
--             -- FIXME Ensure with high prob that inName is actually used
--             quanType <- elements [ ForAll, Exists ]
--             let quanTop = EQuan quanType (BOver (EQVar inName) (EDom dom) )
--
--             quanGuard <- oneof [
--                 return EEmptyGuard, boolExprUsingRef s''  inName
--                 ]
--             quanBody <- boolExpr s''{depth_=depth_ s''  - 1}
--             return $ quanTop quanGuard quanBody
--
--     where
--         overs =  domOf s [TSet TAny, TInt]
--
--
-- quanSum  GG Expr
-- quanSum s =
--     case overs of
--         Nothing -> intLit s
--         Just gen -> do
--             over@(EVar overName) <- gen
--             let overType = lookupType s overName
--
--             let inType =  quanType_in overType
--             let (s', inName) = nextQuanVarName s
--             let s'' = introduceVariable s' (inName, inType)
--
--             let quanTop = EQuan Sum (BIn (EQVar inName) over)
--
--             quanGuard <- oneof [
--                 return EEmptyGuard
--                 ]
--
--             quanBody <-  exprOf s''{depth_=  depth_ s'' -1 } TInt
--             return $ quanTop quanGuard quanBody
--
--     where
--     overs =  varOf s (TSet TInt)

-- -- assuming depth > 1 left
-- boolExprUsingRef  Ref -> GG Expr
-- boolExprUsingRef s@SS{..} ref | depth_ > 1= do
--     let refType = lookupType s ref
--     sidesType <- typeFromType s refType
--
--     other <- exprOf s sidesType
--     refExpr <- exprFromToType s{depth_ = min 2 depth_} ref refType sidesType
--
--     onLeft :: Bool <- arbitrary
--     op <- boolOpFor sidesType
--     if onLeft then
--         return $ op refExpr other
--     else
--         return $ op other refExpr
--
-- -- Types that can be reached from a type in n levels of nesting
-- exprFromToType  Ref -> Type -> Type -> GG Expr
-- exprFromToType _ ref from to | from == to =  return $ EVar ref
--
-- exprFromToType s ref (TSet _) TInt = return $ EUniOp $ UBar $ EVar ref

-- Return a expr of the specifed depth and type
exprOf :: Type -> GG Expr
exprOf ty = do
    ofType <-  map lift . maybeToList <$> varOf ty

    -- Simple cases
    gets depth_ >>= \d -> if
        | d < 0 -> ggError "exprOf depth_ <0" ["exprDom:" <+> pretty ty]
        | d == 0 && ty == TInt  -> oneof2 $ boolLit : ofType
        | d == 0 && ty == TBool -> oneof2 $ intLit  : ofType
        | d < 1 -> ggError "exprOf depth_ <1" ["exprDom:" <+> pretty ty]
        | otherwise  -> exprOf' ty


    where
    exprOf' :: Type -> GG Expr
    exprOf' d  = ggError "exprOf not Matched other"
        ["exprDom:" <+> pretty d]


-- exprOf  Type -> GG Expr
-- exprOf s ty | tracef "exprOf" [pretty ty, prettyDepth s] = undefined
--
-- exprOf s@SS{depth_} d  | depth_ < 0 =  docError
--     ["exprOf depth_ <0 ", "exprDom:" <+> pretty d, pretty . groom $ s]
--
-- exprOf s@SS{depth_=0,..} d@TBool = oneof $ ofType ++
--     [
--       boolLit s
--     ]
--     where ofType = maybeToList $ varOf s d
--
-- exprOf s@SS{..} d@TBool = oneof $ ofType ++
--     [
--       boolLit s
--     , equivExpr s
--     , relationExpr s
--     ]
--     where ofType = maybeToList $ varOf s d
--
--
-- exprOf s@SS{depth_=0,..} d@TInt = oneof $ ofType ++
--     [
--       intLit s
--     ]
--     where ofType = maybeToList $ varOf s d
--
--
-- exprOf s@SS{depth_=1,..} d@TInt = oneof $ ofType ++
--     [
--       intLit s
--     , arithmeticExprOf s d
--     ]
--     where
--     ofType = maybeToList $ varOf s d
--
-- exprOf s@SS{..} d@TInt = oneof $ ofType ++
--     [
--       intLit s
--     , arithmeticExprOf s d
--     -- , quanSum s
--     ]
--     where
--     ofType = maybeToList $ varOf s d
--
-- exprOf s@SS{..} d@(TSet inner) | depth_ >=1 = oneof $ ofType ++
--     [
--        setLitOf s inner
--     ]
--     where ofType = maybeToList $ varOf s d
--
-- exprOf s@SS{..} d@(TMSet inner) | depth_ >=1 = oneof $ ofType ++
--     [
--        msetLitOf s inner
--     ]
--     where ofType = maybeToList $ varOf s d
--
--
-- exprOf s@SS{..} d@(TMatix inner) | depth_ >=1  = oneof $ ofType ++
--     [
--        matrixLitOf s inner
--     ]
--     where ofType = maybeToList $ varOf s d
--
-- exprOf s@SS{..} d@(TFunc a b) | depth_ >=1  = oneof $ ofType ++
--     [
--        funcLitOf s a b
--     ]
--     where ofType = maybeToList $ varOf s d
--
-- exprOf s@SS{..} d@(TRel tys)  | depth_ >=2  = oneof $ ofType ++
--     [
--        relLitOf s tys
--     ]
--     where ofType = maybeToList $ varOf s d
--
-- exprOf s@SS{..} d@(TPar inner)  | depth_ >=1  = oneof $ ofType ++
--     [
--        parLitOf s inner
--     ]
--     where ofType = maybeToList $ varOf s d
--
-- exprOf s@SS{..} d@(TTuple tys)  | depth_ >=1  = oneof $ ofType ++
--     [
--        tupleLitOf s tys
--     ]
--     where ofType = maybeToList $ varOf s d
--
--
-- exprOf s@SS{..} d@(TUnamed _ )  =  docError
--     ["exprOf not Matched", "exprDom:" <+> pretty d, pretty . groom $ s]
--
-- exprOf s@SS{..} d@(TEnum _ )  =  docError
--     ["exprOf not Matched", "exprDom:" <+> pretty d, pretty . groom $ s]
--
--
-- exprOf s@SS{..} d@(TAny  )  =  docError
--     ["exprOf not Matched", "exprDom:" <+> pretty d, pretty . groom $ s]
--
-- exprOf s d  =  docError
--     ["exprOf not Matched other", "exprDom:" <+> pretty d, pretty . groom $ s]


varOf ::  Type -> GG (Maybe (Gen Expr))
varOf exprType = do
    SS{doms_,newVars_} <- get

    let newVars = map fst $ filter (typesUnify exprType . snd ) $ newVars_

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
toGenExpr f vs =  case (map f vs) of
    [] -> Nothing
    xs -> Just $ elements xs
