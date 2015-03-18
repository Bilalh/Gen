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

eq :: (Simpler a b) => a -> b -> TestTree
eq = st EQ

testGroup_lt_gt :: forall a b. (Simpler b a, Simpler a b)
                => String -> [(a, b)] -> TestTree
testGroup_lt_gt name ls =
  testGroup name
   [
     testGroup (name ++  "LT") (map ( uncurry (st LT)) ls)
    ,testGroup (name ++  "GT") (map (uncurry (flip (st GT)))  ls)
   ]



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
    , eq_same [essencee| (function() : `function int --> int`) |]
    , eq_same [essencee| ( {} : `set of int`) |]

   ]

  ,testGroup "Expr_gen eq"
   [
     eq_same [essencee| [false] |]
   , eq_same [essencee| [false; int(1,1,1)] |]
   , eq [essencee| [1,2;int(1,2)] |] [essencee| [5,5;int(5,6)] |]
   , eq [essencee| [1,2;int(1,2,2,1)] |] [essencee| [1,2;int(1,2)] |]
   ]


  ,testGroup_lt_gt "exprs"
   [
     ([essencee| false |],          [essencee| false \/ false |])
   , ([essencee| false \/ false |], [essencee| (true \/ true) != true |])
   , ([essencee| 1 in mset(-5, 4)                  |], [essencee| toInt(toInt(true) in mset(-5, 4))  |])
   , ([essencee| toInt(true) in mset(-5, 4)        |], [essencee| toInt(toInt(true) in mset(-5, 4))  |])
   , ([essencee| toInt(toInt(true) in mset(-5, 4)) |], [essencee| toInt(toInt(true) in mset(-5, 4)) = 9 |])
   , ([essencee| {true}|],                             [essencee| preImage(function(true --> false), false) |])
  ]

  ,testGroup_lt_gt "literals"
   [
     ([essencee| {false} |],  [essencee| {false,true} |] )
   , ([essencee| {} |],       [essencee| {false,true} |] )
   , ([essencee| {true} |],   [essencee| {} |] )

   , ([essencee| [1,2] |],    [essencee| [1,2,3] |] )
   , ([essencee| [1] |],     [essencee| [1,2] |] )
   , ([essencee| [5] |],     [essencee| [] |] )

   , ([essencee| [1,2; int(1,3)] |],    [essencee| [1,2,3; int(2,4,6)] |] )

   , ([essencee| function(true = false --> 5) |], [essencee| function(true = false --> 5,  3=1+2 --> 9) |] )
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
