{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module TestGen.Arbitrary.Helpers where

import AST.Imports
import TestGen.Arbitrary.Type
import Language.E hiding(trace)

import Test.QuickCheck


-- | Generates a random length between the specifed bounds.
--   The length depends on the size parameter.
listOfB :: Int -> Int -> Gen a -> Gen [a]
listOfB l u gen = sized $ \n -> do
    k <- choose ( 0 `max` l, u `min` n)
    vectorOf k gen

typesUnify :: Type -> Type -> Bool
typesUnify TAny  _     = True
typesUnify _     TAny  = True
typesUnify TInt  TInt  = True
typesUnify TBool TBool = True

typesUnify (TMatix i1) (TMatix i2) = typesUnify i1 i2
typesUnify (TSet i1)   (TSet i2)   = typesUnify i1 i2
typesUnify (TMSet i1)  (TMSet i2)  = typesUnify i1 i2
typesUnify (TPar i1)   (TPar i2)   = typesUnify i1 i2

typesUnify (TRel i1)     (TRel i2)      = all (uncurry typesUnify)  $ zip i1 i2
typesUnify (TFunc i1 j1) (TFunc i2 j2)  = all (uncurry typesUnify)  $ [(i1, i2), (j1, j2)]

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
typeOfDom DFunc{innerFrom,innerTo} = TFunc (typeOfDom innerFrom) (typeOfDom innerTo)


-- TODO could be a lot more efficient
typesUnifyO  :: Domain -> Domain -> Bool
typesUnifyO a b = typeUnify (toEssence a) (toEssence b)



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
_sample :: Pretty a => Gen a -> IO ()
_sample e  = do
    a <- sample' e
    mapM_  (putStrLn  . show . pretty) a
