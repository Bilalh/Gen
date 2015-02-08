module Main where

import TestGen.Reduce.Args(parseArgs)
import TestGen.Reduce.Reduce
import TestGen.Reduce.FormatResults
import TestGen.Prelude(Pretty(..))

main :: IO ()
main = do
    args <- parseArgs
    putStrLn . show $ pretty $ args
    (_,state) <- reduceMain args
    formatResults state

    putStrLn "<<Finished>>"
