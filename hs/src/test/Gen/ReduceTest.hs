{-# LANGUAGE FlexibleInstances, QuasiQuotes, UndecidableInstances #-}
module Gen.ReduceTest ( tests ) where

import Gen.AST.TH
import Gen.Imports
import Gen.TestPrelude
import Gen.Reduce.Data
import Gen.Reduce.Reduction as R
import Gen.Reduce.Simpler
import Gen.Helpers.SizeOf
import Gen.Helpers.TypeOf
import Gen.Essence.St
import Gen.Essence.Expr()
import Gen.Essence.Type()
import Gen.Essence.Id

tests :: TestTree
tests = testGroup "ReductionTests"

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

  , testGroup "No reductions should happen"
    [
     testGroup "Literals" $ map r_no_reductions e_no_reductions
    ]

  , testGroup "Reductions should not contain self"
    [
     testGroup "Expr" $ map r_reduce_should_not_contain_self
       [
         [essencee| mset(true, true) supsetEq mset(false, true) |]
       , [essencee| (-5, false, false) |]
       , EDom ([domainn| int(2, 3..5) |] :: Domain () Expr)
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
  , qc_tests "QC"
  ]


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
    , [essencee| [true; int(2..2)] = [true, true, false; int(5..6, 7..7)] |]
    , [essencee| mset(false, false) = mset(true, false) |]
    , [essencee| [true; int(2..2)] = [true, true, false; int(5..6, 7..7)] |]
    ]


e_no_reductions :: [Expr]
e_no_reductions =
    [ [essencee| true |]
    , [essencee| (relation() : `relation of (bool)`) |]
    , [essencee| {} |]
    ]

r_no_reductions :: Expr -> TestTree
r_no_reductions a = testCase (pretty a) $
  let res = runner reduce a
  in assertBool ("Found reductions expected None\n" ++ show (prettyArr res) )  (null res)


r_reduce_should_not_contain_self :: Expr -> TestTree
r_reduce_should_not_contain_self a = testCase (pretty a) $
  let res = runner reduce a
  in assertBool ("Reductions contained self\n" ++ show (prettyArr res) )  (all (/=a) res)



r_depth_lt :: Expr -> TestTree
r_depth_lt a = testCase (pretty a) $
          ( all (\b -> depthOf b < depthOf a ) $ runner reduce a ) @?= (True)

r_depth_some_lt :: Expr -> TestTree
r_depth_some_lt a = testCase (pretty a) $
          ( all (\b -> depthOf b <= depthOf a ) &&& any (\b -> depthOf b < depthOf a )
                    $ runner reduce a ) @?= (True,True)

r_depth_leq :: Expr -> TestTree
r_depth_leq a = testCase (pretty a) $
          ( all (\b -> depthOf b <= depthOf a )  $ runner reduce a ) @?= True


r_reduce_simp_leq :: Expr -> TestTree
r_reduce_simp_leq a = testCase (pretty a) $
          ( all (\b -> simpler_leq b a )  $ runner reduce a ) @?= True

r_subterms_simp_leq :: Expr -> TestTree
r_subterms_simp_leq a = testCase (pretty a) $
          ( all (\b -> simpler_leq b a )  $ runner R.subterms a ) @?= True

r_type_leq :: Expr -> TestTree
r_type_leq a = testCase (pretty a) $
          ( all (\b -> simpler_leq (runIdentity . ttypeOf $ b) (runIdentity . ttypeOf $ a) )
                    $ runner reduce a ) @?= True


r_single_depth :: Expr -> TestTree
r_single_depth a =
    let reduced = runner single a
        res  = map (\rr -> (all (\l -> depthOf l == depthOf rr  )  $ runner reduce rr  )  )
               $ reduced

    in testCase (pretty a) $ (and res)  @?= True



simpler_leq :: forall a b. Simpler a b => a -> b -> Bool
simpler_leq a b = let res = (runIdentity $ simpler a b)
                  in  (res == EQ || res == LT)



data Limited a = Limited GenerateConstraint Doc a
    deriving (Eq)


instance (Pretty a, Show a, DepthOf a, GetKey a) => Pretty (Limited a) where pretty = pretty . show
instance (Pretty a, Show a, DepthOf a, GetKey a) => Show (Limited a)   where
 show (Limited ty logs a) = renderSized 100 $ hang "Limited" 4 $ vcat
          [ nn "Groomed :" (groom a)
          , nn "GenTy   :"  (ty)
          , nn "GenDepth:"  (f ty)
          , nn "Pretty  :"  (a)
          , nn "Depth   :"  (depthOf a)
          , nn "Keys    :"  (groom $ sort $  nub2 $ keyList a)
          , hang "logs" 4 logs
          ]

     where f (GType t) = pretty $ depthOf t
           f _         = "Nothing"


instance (Generate a, Reduce a (StateT EState Identity), Simpler a a, DepthOf a, GetKey a, NeedsType a)
    => Arbitrary (Limited a) where
  arbitrary = sized $ \s -> do
    let allowed =  LogFollow

    i <- choose (1,  (max 0 (min s 3)) )
    con <- giveConstraint (Proxy :: Proxy a) i
    (aa,logs) <- runGenerateWithLogs con def{depth=i}

    Limited <$> pure con
            <*> pure (vcat [ msg | (lvl, msg) <- logs , lvl <= allowed ])
            <*> pure aa


qc_tests :: String  -> TestTree
qc_tests title  =
  testGroup title $
   catMaybes $ use_qc  $ [
     no $  testProperty "reduce_should_not_contain_self" $
       \(Limited _ _ (a :: Expr)) -> ( all (\b -> hash b  /= hash a )  $  limitedRun reduce a )

   -- Comp  like [l_1 | l_1 : mset (size 1 ** 0) of bool, true, true]
   -- simplify to [mset(true) | l_1 : mset (size 1 ** 0) of bool]
   -- which has higher depth.
   , no $ testProperty "depth_leq" $
       \(Limited _ _ (a :: Expr)) -> ( all (\b -> depthOf b <= depthOf a )  $ limitedRun reduce a )
   , Just $ testProperty "reduce_simp_leq" $
       \(Limited _ _ (a :: Expr)) ->  ( all (\b -> simpler_leq b a )  $ limitedRun reduce a )
   , Just $ testProperty "subterms_simp_leq" $
       \(Limited _ _ (a :: Expr)) ->  ( all (\b -> simpler_leq b a )  $ limitedRun R.subterms a )
   , Just $ testProperty "type_leq" $
       \(Limited _ _ (a :: Expr)) ->
           ( all (\b -> simpler_leq (runIdentity . ttypeOf $ b) (runIdentity . ttypeOf $ a) )
                     $ limitedRun reduce a )
   , Just $ testProperty "single_depth" $
       \(Limited _ _ (a :: Expr)) ->
           let reduced = limitedRun single a
               res = map (\rr -> (all (\l -> depthOf l <= depthOf rr  )
                                 $ limitedRun reduce rr)) $ reduced
           in and res
   ]



-- To make sure the test finish quickly
limitedRun :: forall a t. (t -> StateT EState Identity [a]) -> t -> [a]
limitedRun f a = take 200 $ runner f a

use_qc :: [Maybe a] -> [Maybe a]
-- use_qc = return []
use_qc xs = xs


runner :: forall a t. (t -> StateT EState Identity a) -> t -> a
runner f ee = do
  let spe   :: Spec   = $never
      state :: EState = newEState spe
      res             = runIdentity $ flip evalStateT state $ f ee
  res


_prop :: Limited Expr -> Bool
_prop (Limited _ _ (a :: Expr)) = ( all (\b -> depthOf b <= depthOf a )  $ limitedRun reduce a )
