module Gen.TestPrelude ( module X, testCase,no,testProperty, NeedsType(..)) where

import Gen.Imports
import Gen.Essence.St
import Gen.Essence.Type ()
import Test.Tasty       as X (TestTree, testGroup)
import Test.Tasty.HUnit as X ((@?=),assertBool)
import Text.PrettyPrint as X (braces)

import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.QuickCheck    as QC
import Test.Tasty.QuickCheck as X(Arbitrary(..),sized,choose,counterexample,elements)

testCase :: Doc -> H.Assertion -> TestTree
testCase x =  H.testCase (renderSized 1000 $ ">" <+> x)

testProperty :: forall a . QC.Testable a => Doc -> a -> TestTree
testProperty x = QC.testProperty (renderSized 1000 $ ">" <+> x)

no :: forall t a. t -> Maybe a
no _ = Nothing


class NeedsType a where
  needsType :: Proxy a -> Bool
  needsType _  = True

  giveConstraint :: Proxy a -> Int -> QC.Gen GenerateConstraint
  giveConstraint _ i =
      if needsType (Proxy :: Proxy a)  then do
             ty :: Type <- runGenerateNullLogs GNone def{depth=i}
             return $ GType ty
         else
             return $ GNone



instance NeedsType (Expr)
instance NeedsType (Constant)
instance NeedsType (Domain () a)
instance NeedsType (AbstractLiteral a) where needsType _ = False
instance NeedsType Type                where needsType _ = False
