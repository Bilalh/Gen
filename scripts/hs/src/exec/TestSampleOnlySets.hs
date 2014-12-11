{-# LANGUAGE NamedFieldPuns #-}
module Main where

import TestGen.Prelude

import TestGen.Arbitrary.Arbitrary(WithLogs, arbitraryDef, WithExtra)
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
                               ]
        , gen_useFunc = myUseFunc2
        }
    

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
myUseFunc2 Aimage        = False
myUseFunc2 _             = True

