{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}

{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-cse #-} 
-- cse means output is not outputted

module TestGen.QCDebug where

import TestGen.Arbitrary.Arbitrary
import TestGen.Arbitrary.Domain
import TestGen.Arbitrary.Type(atype_only)
import TestGen.Prelude

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Property as QC

import Text.Groom(groom)


newtype S2 = S2 SpecE
    deriving Show  

instance Arbitrary S2 where
    arbitrary = arbitraryDef undefined

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


newtype S3 = S3 SpecE
    deriving Show  

instance Arbitrary S3 where
    arbitrary = arbitraryDef undefined

instance ArbSpec S3 where
    getSpec (S3 sp) = sp
    wrapSpec sp     = S3 sp
    
    tyGens _ = def{
            gen_useFunc = myUseFunc2
        ,   gen_atype   = atype_only [TInt, TBool, TFunc TInt TBool, TSet TAny]  
        ,   gen_dom     = dom_only [boolDomChoice, setDomChoice ]
        }

