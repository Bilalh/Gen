module Main where

import TestGen(main')
import Args(parseArgs)

main :: IO ()
main = do
    globalState <- parseArgs
    main' globalState
    putStrLn "<<Finished>>"



