module Main where

import TestGen.Reduce.Args(parseArgs)
import TestGen.Reduce.Reduce
import TestGen.Prelude(Pretty(..))

main :: IO ()
main = do
    args <- parseArgs
    putStrLn . show $ pretty $ args
    sp <- reduceMain args
    putStrLn "<<Finished>>"
