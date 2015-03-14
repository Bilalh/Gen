{-# LANGUAGE FlexibleInstances, QuasiQuotes #-}
module Gen.ReduceTest ( tests ) where

import Gen.Arbitrary.Generators
import Gen.Arbitrary.Type
import Gen.AST.TH
import Gen.Prelude
import Gen.Reduce.Data
import Gen.Reduce.Reduction
import Gen.Reduce.Simpler
import Test.Tasty               (TestTree, testGroup)
import Test.Tasty.HUnit         (testCase, (@?=))
import Test.Tasty.QuickCheck    as QC



use_qc :: [Maybe a] -> [Maybe a]
-- use_qc = return []
use_qc xs = xs

no _ = Nothing


tests :: TestTree
tests = testGroup "simpler"

  [ testGroup "Expr_gen"
   [
      r_depth_leq [essencee| false |]
    , r_depth_leq [essencee| false \/ false |]
    , r_depth_leq [essencee| false != true |]
    , r_depth_leq [essencee| 4 = -5 |]
    , r_depth_leq [essencee| function(0 --> 3) = function(1 --> 7, 10 --> 6) |]
    , r_depth_leq [essencee| partition({4}) != partition({7}, {3}) |]
    , r_depth_leq [essencee| {true} = {true,true} |]
    , r_depth_leq [essencee| (true \/ true) != true |]
    , r_depth_leq [essencee| 10 = 7 \/ (7 % 10 = 3) |]
    , r_depth_leq [essencee| 2 != 2 /\ (true \/ true) |]
    , r_depth_leq [essencee| preImage(function(true --> true), false) |]
    , r_depth_leq [essencee| toInt(toInt(true) in mset(-5, 4)) = 9 |]
   ]


  ]

__runner :: forall a t. (t -> StateT EState Identity a) -> t -> a
__runner f ee = do
  let spe   :: Spec   = $never
      seed            = 32
      state :: EState = newEStateWithSeed seed spe
      res             = runIdentity $ flip evalStateT state $ f ee
  res

st :: (Simpler a b) => Ordering -> a -> b -> TestTree
st ord a b = testCase (show $ pretty a <+> pretty b) $
  (runIdentity $ simpler a b) @?= ord

r_depth_leq :: Expr -> TestTree
r_depth_leq a = testCase (show $ pretty a) $
          ( all (\b -> depthOf b <= depthOf a )  $ __runner reduce a ) @?= True
