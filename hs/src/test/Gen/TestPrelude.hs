module Gen.TestPrelude ( module X, testCase,no,testProperty) where

import Gen.Imports
import Test.Tasty       as X (TestTree, testGroup)
import Test.Tasty.HUnit as X ((@?=))
import Text.PrettyPrint as X (braces)

import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.QuickCheck    as QC
import Test.Tasty.QuickCheck as X(Arbitrary(..),sized,choose,counterexample)

testCase :: Doc -> H.Assertion -> TestTree
testCase x =  H.testCase (renderSized 1000 $ ">" <+> x)

testProperty :: forall a . QC.Testable a => Doc -> a -> TestTree
testProperty x = QC.testProperty (renderSized 1000 $ ">" <+> x)

no :: forall t a. t -> Maybe a
no _ = Nothing
