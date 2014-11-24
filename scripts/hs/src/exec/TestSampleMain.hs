module Main where

import TestGen.Helpers.Args
import TestGen.QC

main :: IO ()
main = do
    args <- parseArgs
    print args

    generateSpecs args

    putStrLn "<<Finished>>"
