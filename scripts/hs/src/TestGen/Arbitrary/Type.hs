{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module TestGen.Arbitrary.Type where

import TestGen.Prelude

-- return the type of a, knowing b  from  `a in b`
quanType_in :: Type -> Type
quanType_in (TSet inner) = inner


atype_only :: [Type] -> GG Type 
atype_only tys = do 
    addLog "atype_only" ["start"]
    
    d <- gets depth_
    if | d < 0  -> ggError "atype_only invalid depth" []
       | otherwise -> do 
    
        let inDepth = filter (\t -> (fromInteger $ depthOf t) <= d ) tys
        choice <- oneof2 (map converted inDepth)
        return choice

    where 
        converted :: Type -> GG Type
        converted (TMatix TAny)  = liftM TMatix (withDepthDec atype)
        converted (TSet TAny)    = liftM TSet (withDepthDec atype)
        converted (TMSet TAny)   = liftM TMSet (withDepthDec atype)
        converted (TPar TAny)    = liftM TPar (withDepthDec atype)
        
        converted (TFunc TAny TAny) = return TFunc
                    `ap`  (withDepthDec atype)
                    `ap`  (withDepthDec atype)
        
        converted (TFunc TAny b) = return TFunc
                    `ap`  (withDepthDec atype)
                    `ap`  (return b)

        converted (TFunc a TAny) = return TFunc
                    `ap`  (return a)
                    `ap`  (withDepthDec atype)

        converted (TTuple [TAny]) = atuple
        converted (TRel   [TAny]) = arel
        converted ty = return ty 


atype_def :: GG Type
-- TODO partition should also be of depth 2?
atype_def = do
    addLog "atype_def" ["start"]
    d <- gets depth_
    addLog "atype_def" ["depth_" <+> pretty d]


    res <- if
        | d < 0  -> ggError "atype_def invalid depth" []
        | d == 0 -> elements2 [TBool, TInt]
        | d == 1 -> do
            let inner = withDepth 0
            oneof2 [
                  elements2 [TBool, TInt]
                , liftM TMatix (inner atype_def)
                , liftM TSet  (inner atype_def)
                , liftM TMSet (inner atype_def)
                , liftM TPar  (inner atype_def)
                , return TFunc
                    `ap`  (inner atype_def)
                    `ap`  (inner atype_def)
                , atuple
                ]

        | otherwise -> do
            let inner = withDepth (d - 1)
            oneof2 [
                  elements2 [TBool, TInt]
                , liftM TMatix (inner atype_def)
                , liftM TSet  (inner atype_def)
                , liftM TMSet (inner atype_def)
                , liftM TPar  (inner atype_def)
                , return TFunc
                    `ap`  (inner atype_def)
                    `ap`  (inner atype_def)
                , atuple
                , arel
                ]
    d' <- gets depth_
    addLog "atype_def" ["resTy" <+> pretty res, "depth_" <+> pretty d' ]
    return res

atuple :: GG Type
atuple = do
    depth_ <- gets depth_
    addLog "atuple" ["depth_" <+> pretty depth_]

    vs <- listOfBounds (1,  min 10 (2 * depth_))
        (withDepth (depth_ - 1) atype_def )
    return $ TTuple vs

-- a relation e.g   relation (  tuple(int,int) )
-- has a nesting of 2  int -> tuple -> relation

arel :: GG Type
arel = do

    d <- gets depth_
    addLog "arel" ["depth_" <+> pretty d]

    vs <- listOfBounds (1,  min 5 (2 * d))
        (withDepth (d - 2) atype_def )

    return $ TRel vs



typesUnify :: Type -> Type -> Bool
typesUnify TAny  _     = True
typesUnify _     TAny  = True
typesUnify TInt  TInt  = True
typesUnify TBool TBool = True

typesUnify (TMatix i1) (TMatix i2) = typesUnify i1 i2
typesUnify (TSet i1)   (TSet i2)   = typesUnify i1 i2
typesUnify (TMSet i1)  (TMSet i2)  = typesUnify i1 i2

typesUnify (TPar i1)   (TPar i2)   = typesUnify i1 i2

typesUnify (TRel i1)   (TRel i2)   | length i1 == length i2  =
    all (uncurry typesUnify)  $ zip i1 i2
typesUnify (TTuple i1) (TTuple i2) | length i1 == length i2 =
    all (uncurry typesUnify)  $ zip i1 i2

typesUnify (TFunc i1 j1) (TFunc i2 j2)  =
    all (uncurry typesUnify)  $ [(i1, i2), (j1, j2)]

typesUnify (TUnamed t1) (TUnamed t2) = t1 == t2
typesUnify (TEnum t1)   (TEnum t2)   = t1 == t2
typesUnify _ _ = False
