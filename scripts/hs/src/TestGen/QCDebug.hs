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


newtype S1 = S1 SpecE
    deriving Show  

instance Arbitrary S1 where
    arbitrary = arbitraryDef undefined

instance ArbSpec S1 where
    getSpec (S1 sp) = sp
    wrapSpec sp     = S1 sp
    
    tyGens _ = def{
          gen_atype = atype_only [ TBool, TSet TAny  ]
        , gen_dom   = dom_only [boolDomChoice]
        , gen_useFunc = myUseFunc
        }

myUseFunc :: FuncsNames -> Bool 
myUseFunc Aunion = True
myUseFunc _      = False


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
myUseFunc2 _             = True
