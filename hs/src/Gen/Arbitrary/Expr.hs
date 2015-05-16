{-# LANGUAGE QuasiQuotes, ViewPatterns, TupleSections#-}
module Gen.Arbitrary.Expr where

import Gen.Arbitrary.Prelude
import Gen.AST.TH
import Gen.Arbitrary.Type
import Gen.Arbitrary.Common

import {-# SOURCE #-} Gen.Arbitrary.Literal
import {-# SOURCE #-} Gen.Arbitrary.Op
import {-# SOURCE #-} Gen.Arbitrary.FromNested
import {-# SOURCE #-} Gen.Arbitrary.TypeConversions

import Test.QuickCheck(arbitrary,Gen,elements)

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

            over@(EVar (Var overName overType)) <- lift gen
            addLog "quanInExpr" [nn "over" (overName, overType) ]

            let inType =  quanType_in overType
            inName <-  nextQuanVarName
            introduceVariable (Var inName inType)

            addLog "quanInExpr" [ "in" <+> pretty inName
                                , "inTy" <+> pretty inType
                                ]

            -- FIXME Ensure with high prob that inName is actually used
            quanType <- elements2 [ ForAll, Exists ]
            let quanTop = EQuan quanType (Var inName inType) over

            d <- gets depth_
            let typeDepth = depthOf inType
            let useGuardExpr = if
                    | typeDepth < fromIntegral d  ->
                        [withDepthDec $ boolExprUsingRef  (Var inName inType)]
                    | otherwise -> []

            addLog "quanInExpr" ["inDepth" <+> pretty typeDepth ]


            quanGuard <- oneof2 $ [ return EEmptyGuard ] ++ useGuardExpr
            quanBody <- withDepthDec boolExpr
            return $ quanTop quanGuard quanBody

    where
        overs =  varsOf'  (TypeSet TypeAny)

quanOverExpr :: GG Expr
quanOverExpr = withQuan $
    overs >>= \case
        Nothing -> boolExpr  -- Nothing to quantify over
        Just gen -> do
            addLog "quanOverExpr" []
            dm <- lift gen
            let overType = typeOfDom dm

            let innerType = overType
            inName <-  nextQuanVarName
            introduceVariable (Var inName innerType)

            addLog "quanOverExpr" [ "in" <+> pretty inName
                                  , "inTy" <+> pretty innerType
                                  ]

            -- FIXME Ensure with high prob that inName is actually used
            quanType <- elements2 [ ForAll, Exists ]
            let quanTop = EQuan quanType (Var inName innerType) (EDom dm)

            d <- gets depth_
            let typeDepth = depthOf innerType
            let useGuardExpr = if
                    | typeDepth < fromIntegral d  ->
                        [withDepthDec $ boolExprUsingRef  (Var inName innerType)]
                    | otherwise -> []

            addLog "quanOverExpr" ["inDepth" <+> pretty typeDepth ]

            quanGuard <- oneof2 $ [ return EEmptyGuard ] ++ useGuardExpr
            quanBody <- withDepthDec boolExpr
            return $ quanTop quanGuard quanBody

    where
        overs = domOf [TypeSet TypeAny, TypeInt]


quanSum :: GG Expr
quanSum = withQuan $
    overs >>= \case
        Nothing  -> intLit
        Just gen -> do
            addLog "quanSum" []

            over@(EVar (Var overName overType)) <- lift gen

            let inType =  quanType_in overType
            inName <- nextQuanVarName
            introduceVariable  (Var inName inType)

            addLog "quanSum" [nn "over" (overName, overType) ]


            let quanTop = EQuan Sum (Var inName overType) over

            quanGuard <- oneof2 [
                return EEmptyGuard
                ]

            quanBody <-  withDepthDec $ exprOf TypeInt
            return $ quanTop quanGuard quanBody

    where
    overs = varsOf'  (TypeSet TypeInt)


-- assuming depth > 1 left
boolExprUsingRef :: Var -> GG Expr
boolExprUsingRef var@(Var ref refType) = do
    d <- gets depth_
    addLog "boolExprUsingRef" ["depth_" <+> pretty d, "ref" <+> pretty ref]

    sidesType <- typeFromType refType

    addLog "boolExprUsingRef" ["refType" <+> pretty refType
                              , "sidesType" <+> pretty sidesType]


    other <- exprOf sidesType
    refExpr <- withDepth (min 2 d) $ exprFromToType var sidesType

    onLeft :: Bool <- lift arbitrary
    op <- boolOpFor sidesType
    if onLeft then
        return $ op refExpr other
    else
        return $ op other refExpr

-- Types that can be reached from a type in n levels of nesting
exprFromToType :: Var ->Type -> GG Expr
exprFromToType var@(Var _ from) to | from == to =  return $ EVar var
exprFromToType var@(Var _ (TypeSet _)) TypeInt = return  [essencee| |&v| |]
  where v = EVar var

exprFromToType var to  = lineError $line [nn "var" var, nn "to" to]


-- Return a expr of the specifed depth and type
exprOf ::Type -> GG Expr
exprOf ty = do
    nestedOfType <-  maybeToList <$> nestedVarsOf ty
    -- nestedOfType <-  return []
    ofType <-  varsOf ty
    -- ofType <-  return []
    tyCons <-   maybeToList <$> toTypeWithConversions ty
    -- tyCons <-   return []

    let refs = (ofType ++ nestedOfType ++ tyCons )

    d <- gets depth_
    addLog "exprOf" ["depth_" <+> pretty d, "ty"  <+> pretty ty ]

    -- Simple cases
    if
        | d < 0 -> ggError "exprOf depth_ <0" ["exprTy:" <+> pretty ty]
        | d == 0 && ty == TypeInt  -> oneof2 $ intLit : refs
        | d == 0 && ty == TypeBool -> oneof2 $ boolLit  : refs
        | d == 0 && ty == TypeAny  -> oneof2 $ [intLit, boolLit ] ++ refs
        | d == 0 -> ggError "exprOf depth_ <1" ["exprTy:" <+> pretty ty]
        | otherwise  ->  do
            addLog "exprOf" [nn "ty" ty, nn "depth" d ]
            exprOf' d refs ty


    where
    exprOf' :: Depth -> [GG Expr] ->Type -> GG Expr

    exprOf' _ ofType TypeBool = oneof2 $ ofType ++ [
          boolLit
        , equivExpr
        , relationExpr
        ]

    exprOf' 1 ofType TypeInt = oneof2 $ ofType ++ [
          intLit
        , arithmeticExprOf ty
        ]

    exprOf' _ ofType TypeInt = oneof2 $ ofType ++ [
          intLit
        , arithmeticExprOf ty
        , quanSum
        ]

    exprOf' _ ofType (TypeSet inner) = oneof2 $ ofType ++ [
          setLitOf inner
        ]

    exprOf' _ ofType (TypeMSet inner) = oneof2 $ ofType ++ [
          msetLitOf inner
        ]

    exprOf' _ ofType (TypeMatrix _ inner) = frequency2 $ (map (\t -> (10,t)) ofType ) ++ [
          (1,matrixLitOf inner)
        ]

    exprOf' _ ofType (TypeFunction a b) = oneof2 $ ofType ++ [
          funcLitOf a b
        ]

    exprOf' _ ofType (TypeRelation tys) = oneof2 $ ofType ++ [
          relLitOf tys
        ]

    exprOf' _ ofType (TypePartition inner) = oneof2 $ ofType ++ [
          parLitOf inner
        ]

    exprOf' _ ofType (TypeTuple tys) = oneof2 $ ofType ++ [
          tupleLitOf tys
        ]

    exprOf' d _ _  = ggError "exprOf not Matched other"
        ["exprDom:" <+> pretty ty, "d:" <+> pretty d ]


-- Remove one level of any
-- e.g for sets
deAny ::Type -> GG Type
deAny TypeAny = do
    d <- gets depth_
    addLog "deAny" [nn "depth" d]
    ty <- withSameDepth atype
    addLog "deAny" [nn "depth" d, nn "ty" ty]
    return ty

deAny (TypeSet a)       = return TypeSet   <*> (withDepthDec $ deAny a)
deAny (TypeMSet a)      = return TypeMSet  <*> (withDepthDec $ deAny a)
deAny (TypeMatrix x a)  = return (TypeMatrix x) <*> (withDepthDec $ deAny a)
deAny (TypePartition a) = return TypePartition   <*> (withDepthDec $ deAny a)

deAny (TypeTuple ts)      =  return TypeTuple <*> (mapM (withDepthDec . deAny) ts)
deAny (TypeRelation   ts) =  do
    d <- gets depth_
    return TypeRelation  <*> (mapM (withDepth (d - 2) . deAny) ts)

deAny (TypeFunction a b) = return TypeFunction
                     <*> (withDepthDec $ deAny a)
                     <*> (withDepthDec $ deAny b)

deAny ty = return ty


purgeAny ::Type -> GG Type
purgeAny TypeAny = do
    d <- gets depth_
    addLog "purgeAny" [nn "depth" d]
    ty <- withSameDepth atype
    addLog "purgeAny" [nn "depth" d, nn "ty" ty]
    return ty

purgeAny ts@TypeInt        = return ts
purgeAny ts@TypeBool       = return ts
purgeAny ts@(TypeUnnamed _) = return ts
purgeAny ts@(TypeEnum _)   = return ts


purgeAny (TypeSet   ts)       = return TypeSet   <*> (withDepthDec $ purgeAny ts )
purgeAny (TypeMSet  ts)       = return TypeMSet  <*> (withDepthDec $ purgeAny ts )
purgeAny (TypeMatrix x ts)    = return (TypeMatrix x) <*> (withDepthDec $ purgeAny ts )
purgeAny (TypePartition   ts) = return TypePartition   <*> (withDepthDec $ purgeAny ts )

purgeAny (TypeTuple ts)    =  return TypeTuple <*> (mapM (withDepthDec . purgeAny ) ts)
purgeAny (TypeRelation ts) =  do
    d <- gets depth_
    return TypeRelation  <*> (mapM (withDepth (d - 2) . purgeAny) ts)

purgeAny (TypeFunction a b)   = return TypeFunction <*> (withDepthDec $ purgeAny a)
                             <*> (withDepthDec $ purgeAny b)

purgeAny x = lineError $line [nn "x" x]

exprOfPurgeAny ::Type -> GG Expr
exprOfPurgeAny ty  = do
    addLog "exprOfPurgeAny" []
    newTy <- purgeAny ty
    addLog "exprOfPurgeAny" [nn "ty" ty, nn "newTy" newTy]
    exprOf newTy


-- at most one element
-- zero elements if beConstant_
varsOf ::Type -> GG [GG Expr]
varsOf ty = gets beConstant_ >>= \case
    False -> return []
    True  -> map lift . maybeToList <$> varsOf' ty


varsOf' ::Type -> GG (Maybe (Gen Expr))
varsOf' exprType = do
    SS{doms_,newVars_} <- get

    let newVars = filter (\(Var _ ty) -> typesUnify [exprType, ty]   ) newVars_

    return $ toGenExpr EVar $ newVars ++ ( map ( uncurry Var) .
        M.toList  . M.filter (\x -> typesUnify [exprType,x] ) . M.map (typeOfDom . domOfGF )
        )  doms_



domOf ::  [Type] -> GG (Maybe (Gen (Domainn Expr)))
domOf exprTypes = do
    doms_ <- gets doms_
    return $ toGenExpr id  $ (map (domOfGF . snd) . M.toList  .
        M.filter (  (\t -> any (\x -> typesUnify [t, x]) exprTypes )  . typeOfDom . domOfGF ))
            doms_


toGenExpr ::  (a -> b) -> [a] -> Maybe (Gen b)
toGenExpr f vs =  case map f vs of
    [] -> Nothing
    xs -> Just $ elements xs
