module Main where

import TestGen.Classify.Sorter(sorterMain)

main :: IO ()
main = do
    sorterMain
    putStrLn "<<Finished>>"
