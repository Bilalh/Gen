module Gen.TestPrelude ( module X, testCase,no) where

import Gen.Imports
import Test.Tasty       as X (TestTree, testGroup)
import Test.Tasty.HUnit as X ((@?=))
import Text.PrettyPrint as X (braces)

import qualified Test.Tasty.HUnit as H

testCase :: Doc -> H.Assertion -> TestTree
testCase x =  H.testCase (renderSized 1000 $ ">" <+>  x)

no :: forall t a. t -> Maybe a
no _ = Nothing
