{-# LANGUAGE FlexibleInstances, QuasiQuotes #-}
module Gen.ReduceTest ( tests ) where

import Gen.AST.TH
import Gen.Imports
import Gen.TestPrelude
import Gen.Reduce.Data
import Gen.Reduce.Reduction as R
import Gen.Reduce.Simpler
import Gen.Helpers.SizeOf
import Gen.Helpers.TypeOf

_use_qc :: [Maybe a] -> [Maybe a]
-- use_qc = return []
_use_qc xs = xs


tests :: TestTree
tests = testGroup "reduce"

  [ testGroup "Expr Gen"
    [
      testGroup "Simpler subterms <=" (map r_subterms_simp_leq gen_exprs)
    , testGroup "DepthOf reduce <=" (map r_depth_leq gen_exprs)
    , testGroup "Simpler reduce <=" (map r_reduce_simp_leq gen_exprs)
    , testGroup "TypeOf <="  (map r_type_leq gen_exprs)
    , testGroup "Reducing Single can not decrease the depth "  (map r_single_depth gen_exprs)
    ]

  , testGroup "Reducing produces exprs of stictly lower depth"
    [
     testGroup "Ops" $ map r_depth_lt
       [
         [essencee| true /\ false |]
       , [essencee| true /\ false /\ true|]
       ]
    ]

  , testGroup "Reducing produces exprs of <= depth"
    [
     testGroup "Ops" $ map r_depth_some_lt
       [
         [essencee| 10 = 7 \/ (7 % 10 = 3) |]
       , [essencee| preImage(function(true --> true), false) |]
       , [essencee| toInt(toInt(true) in mset(-5, 4)) = 9 |]
       ]
    ]

  ]

__runner :: forall a t. (t -> StateT EState Identity a) -> t -> a
__runner f ee = do
  let spe   :: Spec   = $never
      seed            = 32
      state :: EState = newEStateWithSeed seed spe
      res             = runIdentity $ flip evalStateT state $ f ee
  res

gen_exprs :: [Expr]
gen_exprs =
    [ [essencee| false |]
    , [essencee| false \/ false |]
    , [essencee| false != true |]
    , [essencee| 4 = -5 |]
    , [essencee| function(0 --> 3) = function(1 --> 7, 10 --> 6) |]
    , [essencee| partition({4}) != partition({7}, {3}) |]
    , [essencee| {true} = {true,true} |]
    , [essencee| (true \/ true) != true |]
    , [essencee| 10 = 7 \/ (7 % 10 = 3) |]
    , [essencee| 2 != 2 /\ (true \/ true) |]
    , [essencee| preImage(function(true --> true), false) |]
    , [essencee| toInt(toInt(true) in mset(-5, 4)) = 9 |]
    ]



r_depth_lt :: Expr -> TestTree
r_depth_lt a = testCase (pretty a) $
          ( all (\b -> depthOf b < depthOf a ) $ __runner reduce a ) @?= (True)

r_depth_some_lt :: Expr -> TestTree
r_depth_some_lt a = testCase (pretty a) $
          ( all (\b -> depthOf b <= depthOf a ) &&& any (\b -> depthOf b < depthOf a )
                    $ __runner reduce a ) @?= (True,True)

r_depth_leq :: Expr -> TestTree
r_depth_leq a = testCase (pretty a) $
          ( all (\b -> depthOf b <= depthOf a )  $ __runner reduce a ) @?= True


r_reduce_simp_leq :: Expr -> TestTree
r_reduce_simp_leq a = testCase (pretty a) $
          ( all (\b -> simpler_leq b a )  $ __runner reduce a ) @?= True

r_subterms_simp_leq :: Expr -> TestTree
r_subterms_simp_leq a = testCase (pretty a) $
          ( all (\b -> simpler_leq b a )  $ __runner R.subterms a ) @?= True

r_type_leq :: Expr -> TestTree
r_type_leq a = testCase (pretty a) $
          ( all (\b -> simpler_leq (runIdentity . ttypeOf $ b) (runIdentity . ttypeOf $ a) )
                    $ __runner reduce a ) @?= True


r_single_depth :: Expr -> TestTree
r_single_depth a =
    let reduced = __runner single a
        res  = map (\rr -> (all (\l -> depthOf l == depthOf rr  )  $ __runner reduce rr  )  )
               $ reduced

    in testCase (pretty a) $ (and res)  @?= True



simpler_leq :: forall a b. Simpler a b => a -> b -> Bool
simpler_leq a b = let res = (runIdentity $ simpler a b)
                  in  (res == EQ || res == LT)

-- Add tests for mutate for functions
