module Main where

import EssenceSolver.EssenceSolver(main')
import EssenceSolver.Args(parseArgs)

main :: IO ()
main = do
    args <- parseArgs
    main' args
    putStrLn "<<Finished>>"



