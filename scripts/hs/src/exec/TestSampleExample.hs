{-# LANGUAGE NamedFieldPuns #-}
module Main where

import TestGen.Prelude

import TestGen.Arbitrary.Arbitrary(arbitraryDef,WithExtra)
import TestGen.Arbitrary.Domain
import TestGen.Arbitrary.Type(atype_only)
import TestGen.Helpers.Args(parseArgs)
import TestGen.QC(generateSpecs)

main :: IO ()
main = do
    args <- parseArgs
    print args

    generateSpecs (undefined :: WithExtra S1) args
    putStrLn "<<Finished>>"


newtype S1 = S1 SpecE
    deriving Show

instance Arbitrary S1 where
    arbitrary = arbitraryDef undefined

instance ArbSpec S1 where
    getSpec (S1 sp) = sp
    wrapSpec sp     = S1 sp

    tyGens _ = def{
          gen_atype = atype_only [ TInt, TBool, TSet TAny  ]
        , gen_dom   = dom_only [boolDomChoice, setDomChoice
                               , (1 + 1 + 0, myFuncDom)
                               ]
        , gen_useFunc = myUseFunc2
        }

-- Only return domains like function tuple(Any) -> Int
-- Any meaning any of the domains from `dom` i.e `gen_dom`
-- The function may be total
myFuncDom :: GG Domain
myFuncDom = do
    innerFrom <- withDepthDec intDom
    innerTo   <- withDepthDec tupleDom
    total     <- elements2 [True, False]
    return dfunc{innerFrom,innerTo,total}

-- Allows specifying which function to generate
-- see TestGen.Arbitrary.Data for full list

-- These all cause some kind of typechecking error
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
