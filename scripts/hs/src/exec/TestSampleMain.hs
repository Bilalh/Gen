module Main where

import TestGen.Arbitrary.Arbitrary(WithLogs)
import TestGen.Helpers.Args(parseArgs)
import TestGen.QC
import TestGen.Prelude(SpecE)

main :: IO ()
main = do
    args <- parseArgs
    print args

    generateSpecs (undefined :: WithLogs SpecE) args
    putStrLn "<<Finished>>"
