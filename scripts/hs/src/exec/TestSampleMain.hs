module Main where

import TestGen.Helpers.Args
import TestGen.QC

main :: IO ()
main = do
    args <- parseArgs
    print args

    generate args

    putStrLn "<<Finished>>"
