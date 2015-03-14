module Main where

import Gen.Prelude
import Test.Tasty
import Test.Tasty.Runners.AntXML

import qualified Gen.DepthTest   (tests)
import qualified Gen.SimplerTest (tests)

main :: IO ()
main = do
    defaultMainWithIngredients (antXMLRunner : defaultIngredients)
        $ testGroup "gen"
            [ Gen.DepthTest.tests
            , Gen.SimplerTest.tests
            ]
