module Main where

import TestGen.Arbitrary.Arbitrary(WithExtra)
import TestGen.Helpers.Args
import TestGen.QC
import TestGen.Prelude(SpecE)

run :: IO ()
run = do
    let args= TArgs
         { baseDirectory_   = "/Users/bilalh/Desktop/Results/zzz"
         , totalTime_       = 10
         , perSpecTime_     = 2
         , rseed_           = Just 434
         , size_            = 4
         , cores_           = 4
         , typecheckOnly_   = Nothing
         , runToolchain_    = True
         , newConjure_      = True
        }

    generateSpecs (undefined :: WithExtra SpecE) args
    putStrLn "<<Finished>>"
