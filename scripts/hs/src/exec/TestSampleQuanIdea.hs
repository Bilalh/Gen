module Main where

import TestGen.Helpers.Args(parseArgs)
import TestGen.QCUnused
import TestGen.Prelude(def)

main :: IO ()
main = do
    args <- parseArgs
    print args

    generateSpecs2 args def
    putStrLn "<<Finished>>"
