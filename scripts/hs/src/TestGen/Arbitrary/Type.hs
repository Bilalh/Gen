{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module TestGen.Arbitrary.Type where

import AST.Imports
import TestGen.Arbitrary.Data
import TestGen.Arbitrary.Helpers

import Test.QuickCheck
import Language.E

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


atype :: SpecState -> Gen Type
atype s | tracef "atype" [prettyDepth s] = undefined

atype  s@SS{depth_} | depth_ <0   = docError
    ["atype invaild depth", pretty s ]

atype  SS{depth_=0}   = elements [ TBool, TInt ]

atype  s | depth_ s == 1 = oneof [
          elements [ TBool, TInt ]
        , liftM TMatix (atype newss)
        , liftM TSet  (atype newss)
        , liftM TMSet (atype newss)
        , liftM TPar (atype newss)
        , return TFunc
            `ap` (atype newss)
            `ap` (atype newss)
        , atuple s
        ]
    where
        newss = s{depth_=newDepth $ depth_ s}

atype  s = oneof [
          elements [ TBool, TInt ]
        , liftM TMatix (atype newss)
        , liftM TSet  (atype newss)
        , liftM TMSet (atype newss)
        , liftM TPar (atype newss)
        , return TFunc
            `ap` (atype newss)
            `ap` (atype newss)
        , atuple s
        , arel s
        ]
    where
        newss = s{depth_=newDepth $ depth_ s}


newDepth depth
    | depth < 5   =  depth  - 1
    | depth < 20  =  depth - 5
    | otherwise   =  depth `div` 2


atuple s | tracef "atuple" [prettyDepth s] = undefined
atuple ss = do
    vs <- listOfB 1 (min 10 (2 * depth_ ss))
    -- vs <- vectorOf 1
        (atype ss{depth_=newDepth $ depth_ ss})
    return $ TTuple vs

arel s| tracef "arel" [prettyDepth s] = undefined
arel ss = do
    vs <- listOfB 1 (min 5 (2 * depth_ ss))
    -- vs <- vectorOf 1
        (atuple ss{depth_=newDepth $ depth_ ss})
    return $ tracer "arel" ([ pretty $ TRel vs]) (TRel vs)
