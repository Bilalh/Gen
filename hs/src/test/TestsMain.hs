module Main where

import Gen.Imports
import Test.Tasty
import Test.Tasty.Runners.AntXML

import qualified Gen.DepthTest
import qualified Gen.GenerateTest
import qualified Gen.GenerateTypeCheckTest
import qualified Gen.OlderTests
import qualified Gen.ReduceTest
import qualified Gen.SimplerTest
import qualified Solver.SolverTest

main :: IO ()
main = do
    ts <- sequence
          [
            Solver.SolverTest.tests
          ]

    defaultMainWithIngredients (antXMLRunner : defaultIngredients)
        $ testGroup "TopLevel" $
            [ Gen.DepthTest.tests
            , Gen.ReduceTest.tests
            , Gen.SimplerTest.tests
            , Gen.OlderTests.tests
            , Gen.GenerateTest.tests
            , Gen.GenerateTypeCheckTest.tests
            ] ++ ts
