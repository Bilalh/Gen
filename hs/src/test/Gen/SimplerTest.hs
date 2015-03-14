{-# LANGUAGE FlexibleInstances, QuasiQuotes #-}
module Gen.SimplerTest ( tests,use_qc ) where

import Gen.Arbitrary.Generators
import Gen.Arbitrary.Type
import Gen.AST.TH
import Gen.Prelude
import Gen.Reduce.Simpler
import Test.Tasty               (TestTree, testGroup)
import Test.Tasty.HUnit         (testCase, (@?=))
import Test.Tasty.QuickCheck    as QC

st :: (Simpler a b) => Ordering -> a -> b -> TestTree
st ord a b = testCase (show $ pretty a <+> pretty b) $
  (runIdentity $ simpler a b) @?= ord

eq_same :: (Simpler a a) => a -> TestTree
eq_same a = st EQ a a


use_qc :: [a] -> [Maybe a]
-- use_qc = map (\_ -> Nothing)
use_qc = map Just

_qc :: [a] -> [Maybe a]
_qc = map (\_ -> Nothing)

tests :: TestTree
tests = testGroup "simpler"
  [
   testGroup "type"
   [
    eq_same TBool
   ]

  ,testGroup "type complex"
   [
     eq_same (TFunc TInt TBool)
   , eq_same (TSet (TFunc TInt TBool))
   , eq_same (TTuple [TRel [TInt,TBool],TTuple [TTuple [TBool],TTuple [TBool,TInt],TTuple [TInt,TInt],TInt],TTuple [TFunc TBool TInt]])
   ]

  ,testGroup "type_QC" $
   catMaybes $ _qc [
    QC.testProperty "type is equal to self" $
      \(AType a) ->  (runIdentity $ simpler a a) == EQ
   ]

  ]

newtype AType =  AType TType
    deriving (Show,Eq)

instance Arbitrary AType where
    arbitrary = flip evalStateT def{depth_=3} $ fmap AType atype
