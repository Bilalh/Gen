module Main where

import Gen.Imports
import Test.Tasty
import Test.Tasty.Runners.AntXML

import qualified Gen.DepthTest
import qualified Gen.GenerateTest
import qualified Gen.GenerateTypeCheckTest
import qualified Gen.ReduceTest
import qualified Gen.SimplerTest
import qualified Gen.WeightsTest
import qualified Solver.SolverTest
import qualified Instance.PointTest

main :: IO ()
main = do
    ts <- sequence
          [
            -- Solver.SolverTest.tests
          ]

    defaultMainWithIngredients (antXMLRunner : defaultIngredients)
        $ testGroup "TopLevel" $
            items ++ ts

items :: [TestTree]
items = [ Gen.DepthTest.tests
        , Gen.ReduceTest.tests
        , Gen.SimplerTest.tests
        , Gen.GenerateTest.tests
        , Gen.GenerateTypeCheckTest.tests
        , Gen.WeightsTest.tests
        , Instance.PointTest.tests
        ]
