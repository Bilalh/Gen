module Gen.TestPrelude ( module X, testCase) where

import Gen.Prelude
import Test.Tasty       as X (TestTree, testGroup)
import Test.Tasty.HUnit as X ((@?=))
import Text.PrettyPrint as X (braces)

import qualified Test.Tasty.HUnit as H

testCase :: Doc -> H.Assertion -> TestTree
testCase x =  H.testCase (renderSized 1000 $ ">" <+>  x)
