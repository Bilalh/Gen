{-# LANGUAGE NamedFieldPuns #-}
module Main where

import TestGen.Prelude

import TestGen.Arbitrary.Arbitrary(WithLogs, arbitraryDef)
import TestGen.Arbitrary.Domain
import TestGen.Arbitrary.Type(atype_only)
import TestGen.Helpers.Args(parseArgs)
import TestGen.QC(generateSpecs)

main :: IO ()
main = do
    args <- parseArgs
    print args
    
    generateSpecs (undefined :: WithLogs S1) args
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
        , gen_useFunc = myUseFunc
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
myUseFunc :: FuncsNames -> Bool 
myUseFunc Amax   = False
myUseFunc Aparty = False
myUseFunc Aunion = False
myUseFunc Aubar  = False
myUseFunc _      = True


