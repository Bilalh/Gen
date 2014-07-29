module Main where

import TestGen.TestGen(main')
import TestGen.Args(parseArgs)

main :: IO ()
main = do
    globalState <- parseArgs
    main' globalState
    putStrLn "<<Finished>>"



