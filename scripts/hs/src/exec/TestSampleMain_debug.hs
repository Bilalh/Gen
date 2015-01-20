module Main where

import TestGen.Arbitrary.Arbitrary(WithExtra)
import TestGen.Helpers.Args
import TestGen.QC
import TestGen.Prelude(SpecE)

run :: IO ()
run = do
    let args= TArgs
         { baseDirectory_   = "/Users/bilalh/bh246/_a"
         , totalTime_       = 25
         , perSpecTime_     = 10
         , rseed_           = Just 1212
         , size_            = 4
         , cores_           = 1
         , typecheckOnly_   = Nothing
         , runToolchain_    = True
         , newConjure_      = True
        }

    generateSpecs (undefined :: WithExtra SpecE) args
    putStrLn "<<Finished>>"
