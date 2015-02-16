{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-cse #-}
-- cse means output is not outputted

module TestGen.QCDebug where

import TestGen.Arbitrary.Arbitrary
import TestGen.Arbitrary.Domain
import TestGen.Arbitrary.Type(atype_only)
import TestGen.Prelude


newtype S2 = S2 Spec
    deriving Show

instance Arbitrary S2 where
    arbitrary = arbitraryDef $never

instance ArbSpec S2 where
    getSpec (S2 sp) = sp
    wrapSpec sp     = S2 sp

    tyGens _ = def{
            gen_useFunc = myUseFunc2
        }

myUseFunc2 :: FuncsNames -> Bool
myUseFunc2 Atogether     = False
myUseFunc2 Aapart        = False
myUseFunc2 Aparticipants = False
myUseFunc2 Aparty        = False
myUseFunc2 AtoMSet       = False
myUseFunc2 Ahist         = False
myUseFunc2 Aubar         = False
myUseFunc2 Amin          = False
myUseFunc2 Amax          = False
myUseFunc2 Adiff         = False
myUseFunc2 Aunion        = False
myUseFunc2 Aintersect    = False
myUseFunc2 Aimage        = False
myUseFunc2 _             = True


newtype S3 = S3 Spec
    deriving Show

instance Arbitrary S3  where
    arbitrary = arbitraryDef $never

instance ArbSpec S3 where
    getSpec (S3 sp) = sp
    wrapSpec sp     = S3 sp

    tyGens _ = def{
            gen_useFunc = myUseFunc2
        ,   gen_atype   = atype_only [TInt, TBool, TFunc TInt TBool, TSet TAny]
        ,   gen_dom     = dom_only [boolDomChoice, setDomChoice ]
        }


specE1 :: Spec
specE1= readNote "specE1" $
    "SpecE (fromList [(\"var1\",Find (DSet {size = Nothing, minSize = Nothing, maxSize = Nothing, inner = DBool}))]) [EBinOp (BOr (EBinOp (BEQ (ELit (ESet [EExpr (ELit (EB True))])) (ELit (ESet [EExpr (ELit (EB True)),EExpr (ELit (EB True))])))) (ELit (EB False)))]"
