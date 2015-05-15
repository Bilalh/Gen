module Main where

import Gen.Imports
import Test.Tasty
import Test.Tasty.Runners.AntXML

import qualified Gen.DepthTest
import qualified Gen.SimplerTest
import qualified Gen.ReduceTest
import qualified Solver.SolverTest

main :: IO ()
main = do
    ts <- sequence
          [
            Solver.SolverTest.tests
          ]

    defaultMainWithIngredients (antXMLRunner : defaultIngredients)
        $ testGroup "gen" $
            [ Gen.DepthTest.tests
            , Gen.SimplerTest.tests
            , Gen.ReduceTest.tests
            ] ++ ts
