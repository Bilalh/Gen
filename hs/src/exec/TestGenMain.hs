module Main where

import TestGen.Old.TestGen(main')
import TestGen.Old.Args(parseArgs)

main :: IO ()
main = do
    globalState <- parseArgs
    main' globalState
    putStrLn "<<Finished>>"



