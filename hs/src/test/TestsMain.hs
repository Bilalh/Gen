module Main where

import Gen.Imports
import Test.Tasty
import Test.Tasty.Runners.AntXML

import qualified Gen.DepthTest   (tests)
import qualified Gen.SimplerTest (tests)
import qualified Gen.ReduceTest (tests)

main :: IO ()
main = do
    defaultMainWithIngredients (antXMLRunner : defaultIngredients)
        $ testGroup "gen"
            [ Gen.DepthTest.tests
            , Gen.SimplerTest.tests
            , Gen.ReduceTest.tests
            ]
