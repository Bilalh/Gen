{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module TestGen.Arbitrary.Type where

import TestGen.Arbitrary.Helpers.Prelude


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

typeOfDom :: Domain -> Type
typeOfDom  DInt{} = TInt
typeOfDom  DBool  = TBool

typeOfDom DMat{inner}  = TMatix (typeOfDom inner)
typeOfDom DSet{inner}  = TSet   (typeOfDom inner)
typeOfDom DMSet{inner} = TMSet  (typeOfDom inner)
typeOfDom DPar{inner}  = TPar  (typeOfDom inner)

typeOfDom DRel{inners} = TRel (map typeOfDom inners)
typeOfDom DFunc{innerFrom,innerTo} =
    TFunc (typeOfDom innerFrom) (typeOfDom innerTo)

typeOfDom DTuple{inners} = TTuple (map typeOfDom inners)

-- return the type of a, knowing b  from  `a in b`
quanType_in :: Type -> Type
quanType_in (TSet inner) = inner


atype :: GG Type
-- atype s | tracef "atype" [prettyDepth s] = undefined

atype = do
    s@SS{depth_} <- get
    if
        | depth_ < 0  -> docError ["atype invaild depth", pretty s ]
        | depth_ == 0 -> elements2 [TBool, TInt]
        | depth_ == 1 -> do
            let inner = withDepth 0
            oneof2 [
                  elements2 [TBool, TInt]
                , liftM TMatix (inner atype)
                , liftM TSet  (inner atype)
                , liftM TMSet (inner atype)
                , liftM TPar  (inner atype)
                , return TFunc
                    `ap`  (inner atype)
                    `ap`  (inner atype)
                , atuple
                , arel
                ]

        | otherwise -> do
            let inner = withDepth (depth_ - 1)
            oneof2 [
                  elements2 [TBool, TInt]
                , liftM TMatix (inner atype)
                , liftM TSet  (inner atype)
                , liftM TMSet (inner atype)
                , liftM TPar  (inner atype)
                , return TFunc
                    `ap`  (inner atype)
                    `ap`  (inner atype)
                , atuple
                , arel
                ]


atuple :: GG Type
atuple = do
    depth_ <- gets depth_
    vs <- listOfBounds (1,  min 10 (2 * depth_))
        (withDepth (depth_ - 1) atype )
    return $ TTuple vs

-- a relation e.g   relation (  tuple(int,int) )
-- has a nesting of 2  int -> tuple -> relation

arel :: GG Type
arel = do
    depth_ <- gets depth_

    vs <- listOfBounds (1,  min 5 (2 * depth_))
        (withDepth (depth_ - 1) atype )

    return $ (TRel vs)
