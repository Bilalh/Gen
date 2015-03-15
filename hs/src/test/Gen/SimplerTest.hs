{-# LANGUAGE FlexibleInstances, QuasiQuotes #-}
module Gen.SimplerTest ( tests ) where

import Gen.Arbitrary.Generators
import Gen.AST.TH
import Gen.Prelude
import Gen.TestPrelude
import Gen.Reduce.Simpler
import Test.Tasty.QuickCheck    as QC

st :: (Simpler a b) => Ordering -> a -> b -> TestTree
st ord a b = testCase ( pretty a <+> "|" <+> pretty b) $
  (runIdentity $ simpler a b) @?= ord

eq_same :: (Simpler a a) => a -> TestTree
eq_same a = st EQ a a

lt :: (Simpler a b) => a -> b -> TestTree
lt = st LT

gt :: (Simpler a b) => a -> b -> TestTree
gt = st LT



use_qc :: [Maybe a] -> [Maybe a]
-- use_qc = return []
use_qc xs = xs

no :: a -> Maybe a
no _ = Nothing


tests :: TestTree
tests = testGroup "simpler"
  [
   testGroup "type eq"
   [
    eq_same TBool
   ]

  ,testGroup "type complex eq"
   [
     eq_same (TFunc TInt TBool)
   , eq_same (TSet (TFunc TInt TBool))
   , eq_same (TTuple [TRel [TInt,TBool],TTuple [TTuple [TBool],TTuple [TBool,TInt],TTuple [TInt,TInt],TInt],TTuple [TFunc TBool TInt]])
   , eq_same (TRel [TInt,TBool])
   ]

  ,testGroup "type_QC" $
   catMaybes $ use_qc [
     Just $ QC.testProperty "type is equal to self" $
       \(AType a) ->  (runIdentity $ simpler a a) == EQ
   , no $ QC.testProperty "atype and depthOf argee" $
       \(BType ty gen_depth) ->  depthOf ty == fromIntegral gen_depth
   ]

  ,testGroup "Expr_gen eq"
   [
      eq_same [essencee| false |]
    , eq_same [essencee| false \/ false |]
    , eq_same [essencee| false != true |]
    , eq_same [essencee| 4 = -5 |]
    , eq_same [essencee| function(0 --> 3) = function(1 --> 7, 10 --> 6) |]
    , eq_same [essencee| partition({4}) != partition({7}, {3}) |]
    , eq_same [essencee| {true} = {true,true} |]
    , eq_same [essencee| (true \/ true) != true |]
    , eq_same [essencee| 10 = 7 \/ 7 > 10 |]
    , eq_same [essencee| 2 != 2 /\ (true \/ true) |]
    , eq_same [essencee| preImage(function(true --> true), false) |]
    , eq_same [essencee| toInt(toInt(true) in mset(-5, 4)) = 9 |]
   ]

  ,testGroup "Expr_gen LT"
   [
      lt [essencee| false |]          [essencee| false \/ false |]
    , lt [essencee| false \/ false |] [essencee| (true \/ true) != true |]

    , lt [essencee| mset(-5, 4)                       |] [essencee| 1 in mset(-5, 4)  |]
    , lt [essencee| 1 in mset(-5, 4)                  |] [essencee| toInt(toInt(true) in mset(-5, 4))  |]
    , lt [essencee| toInt(true) in mset(-5, 4)        |] [essencee| toInt(toInt(true) in mset(-5, 4))  |]
    , lt [essencee| toInt(toInt(true) in mset(-5, 4)) |] [essencee| toInt(toInt(true) in mset(-5, 4)) = 9 |]

   ]

  ,testGroup "Expr_gen GT"
   [
      (flip gt) [essencee| false |]          [essencee| false \/ false |]
    , (flip gt) [essencee| false \/ false |] [essencee| (true \/ true) != true |]

    , (flip gt) [essencee| mset(-5, 4)                       |] [essencee| 1 in mset(-5, 4)  |]
    , (flip gt) [essencee| 1 in mset(-5, 4)                  |] [essencee| toInt(toInt(true) in mset(-5, 4))  |]
    , (flip gt) [essencee| toInt(true) in mset(-5, 4)        |] [essencee| toInt(toInt(true) in mset(-5, 4))  |]
    , (flip gt) [essencee| toInt(toInt(true) in mset(-5, 4)) |] [essencee| toInt(toInt(true) in mset(-5, 4)) = 9 |]

   ]


  ]

newtype AType =  AType TType
    deriving (Show,Eq)

instance Arbitrary AType where
    arbitrary = flip evalStateT def{depth_=3} $ fmap AType atype


data BType =  BType TType Int
    deriving (Show,Eq)

instance Arbitrary BType where
    arbitrary = sized $ \s -> do
      i <- choose (1,  (max 0 (min s 3)) )
      res <- flip evalStateT def{depth_=i} atype
      return $ BType res i
