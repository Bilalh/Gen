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
-- TODO partition should also be of depth 2?

atype = do
    addLog "atype" ["start"]
    d <- gets depth_
    addLog "atype" ["depth_" <+> pretty d]


    res <- if
        | d < 0  -> ggError "atype invaild depth" []
        | d == 0 -> elements2 [TBool, TInt]
        | d == 1 -> do
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
                ]

        | otherwise -> do
            let inner = withDepth (d - 1)
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
    d' <- gets depth_
    addLog "atype" ["resTy" <+> pretty res, "depth_" <+> pretty d' ]
    return res

atuple :: GG Type
atuple = do
    depth_ <- gets depth_
    addLog "atuple" ["depth_" <+> pretty depth_]

    vs <- listOfBounds (1,  min 10 (2 * depth_))
        (withDepth (depth_ - 1) atype )
    return $ TTuple vs

-- a relation e.g   relation (  tuple(int,int) )
-- has a nesting of 2  int -> tuple -> relation

arel :: GG Type
arel = do

    d <- gets depth_
    addLog "arel" ["depth_" <+> pretty d]

    vs <- listOfBounds (1,  min 5 (2 * d))
        (withDepth (d - 2) atype )

    return $ TRel vs
