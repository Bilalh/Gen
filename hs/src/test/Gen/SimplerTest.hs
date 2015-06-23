{-# LANGUAGE FlexibleInstances, QuasiQuotes, KindSignatures, UndecidableInstances #-}
module Gen.SimplerTest ( tests ) where

import Gen.AST.TH
import Gen.Essence.St
import Gen.Essence.Type ()
import Gen.Essence.Constant ()
import Gen.Essence.Expr ()
import Gen.Helpers.SizeOf
import Gen.Imports
import Gen.Reduce.Simpler
import Gen.Reduce.Data
import Gen.Reduce.Reduction as R
import Gen.TestPrelude
import Text.Printf
import Gen.Essence.Id
import Conjure.Language.Definition

tests :: TestTree
tests = testGroup "simpler"
  [
   testGroup "type eq"
   [
    eq_same TypeBool
   ]

  ,testGroup "type complex eq"
   [
     eq_same (TypeFunction TypeInt TypeBool)
   , eq_same (TypeSet (TypeFunction TypeInt TypeBool))
   , eq_same (TypeTuple [TypeRelation [TypeInt,TypeBool],TypeTuple [TypeTuple [TypeBool],TypeTuple [TypeBool,TypeInt],TypeTuple [TypeInt,TypeInt],TypeInt],TypeTuple [TypeFunction TypeBool TypeInt]])
   , eq_same (TypeRelation [TypeInt,TypeBool])
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
   , ([essencee|  ({} : `set of int`)   |],   [essencee|   ({} : `set of int`) != {5}  |])
   ]

  ,testGroup_lt_gt "Literals"
   [
     ([essencee| {false} |],  [essencee| {false,true} |] )
   , ([essencee| {} |],       [essencee| {false,true} |] )
   , ([essencee| {} |],       [essencee| {true} |] )

   , ([essencee| [1,2] |],    [essencee| [1,2,3] |] )
   , ([essencee| [1] |],      [essencee| [1,2] |] )
   , ([essencee| [] |],       [essencee| [5] |] )

   , ([essencee| [1,2; int(1,3)] |],    [essencee| [1,2,3; int(2,4,6)] |] )

   , ([essencee| function(true = false --> 5) |], [essencee| function(true = false --> 5,  3=1+2 --> 9) |] )

   , ([essencee| or([true])       |], [essencee| or([false,true])       |])
   , ([essencee| or([true])       |], [essencee| or([false,true,false]) |])
   , ([essencee| or([true,false]) |], [essencee| or([false,true,true])  |])

   , ([essencee| or([true/\false]) |], [essencee| or([true/\false,true/\false])|])

   , ([essencee| or([true/\false,true/\false])|], [essencee| or([true/\false,true/\false,true/\false])|] )
  ]

  ,testGroup "Expr_gen Same" $ do
     let q1_Exp = EVar $ Var "q1_Expr" (TypeMatrix TypeInt TypeInt)
     let q1_ExpM = EVar $ Var "q1_ExprM" (TypeInt)
     let q10    = EVar $ Var "q10" (TypeInt)
     let q10s   = Single "q10"
     let q8     = EVar $ Var "q8" (TypeInt)
     [
         eq_same [essencee| [&q1_Exp[&q10] = &q8 | &q10s : int(1..6)]  |]
       , eq_same [essencee| or([&q1_Exp[&q10] = &q8 | &q10s : int(1..6), &q10 <= &q1_ExpM]) |]
       , eq_same [essencee| true -> or([&q1_Exp[&q10] = &q8 | &q10s : int(1..6)])  |]
       , eq_same [essencee| true -> or([&q1_Exp[&q10] = &q8 | &q10s : int(1..6), &q10 <= &q1_ExpM]) |]
       ]

  ,testGroup_lt_gt "Comp" $ do
     let q1_Exp = EVar $ Var "q1_Expr" (TypeMatrix TypeInt TypeInt)
     let q1_ExpM = EVar $ Var "q1_ExprM" (TypeInt)
     let q10    = EVar $ Var "q10" (TypeInt)
     let q10s   = Single "q10"
     let q8     = EVar $ Var "q8" (TypeInt)
     [
         ( [essencee| [&q1_Exp[&q10] = &q8 | &q10s : int(1..6)]  |],
           [essencee| [&q1_Exp[&q10] = &q8 | &q10s : int(1..6), &q10 <= &q1_ExpM] |]
         ),

         ( [essencee| or([&q1_Exp[&q10] = &q8 | &q10s : int(1..6)])  |],
           [essencee| or([&q1_Exp[&q10] = &q8 | &q10s : int(1..6), &q10 <= &q1_ExpM]) |]
         ),

         ( [essencee| true -> or([&q1_Exp[&q10] = &q8 | &q10s : int(1..6)])  |],
           [essencee| true -> or([&q1_Exp[&q10] = &q8 | &q10s : int(1..6), &q10 <= &q1_ExpM]) |]
         )

      ]


   -- , te 1 $ do
   --     let a = Single "l1"
   --     let b = EVar (Var "l1" TypeInt)
   --     [essencee|[ &b | &a : int(1..5, 0)] |]

  ,testGroup "QC"
  [
    qc_tests "Type" (Proxy :: Proxy Type)
  , qc_tests "Constant" (Proxy :: Proxy (Constant))
  , qc_tests "Expr" (Proxy :: Proxy (Expr))
  , qc_tests "Expr" (Proxy :: Proxy (Domain () Expr))
  ]

  ]



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



data BType =  BType Type Int
    deriving (Show,Eq)

instance Arbitrary BType where
  arbitrary = sized $ \s -> do
    i <- choose (1,  (max 0 (min s 3)) )
    BType <$>  runGenerateNullLogs GNone def{depth=i} <*> pure i


data Limited a = Limited GenerateConstraint Doc a
    deriving (Eq)


instance (Pretty a, Show a, DepthOf a, GetKey a) => Pretty (Limited a) where pretty = pretty . show
instance (Pretty a, Show a, DepthOf a, GetKey a) => Show (Limited a)   where
 show (Limited ty logs a) = renderSized 100 $ hang "Limited" 4 $ vcat
          -- [ nn "Groomed :" (groom a)
          [ nn "GenTy   :"  (ty)
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

  shrink (Limited ty logs a ) = do
    let rs = runner reduceSimpler a
    map (Limited ty logs) rs



qc_tests :: forall p
          . (Generate p, Simpler p p, DepthOf p, Reduce p (StateT EState Identity), GetKey p, NeedsType p)
         => String -> Proxy p  -> TestTree
qc_tests title _ =
  testGroup title $
   catMaybes $ use_qc $ [
     Just $ testProperty "Is equal to self" $
       \(Limited _ _ (a :: p)) ->  (runIdentity $ simpler a a) == EQ
   , Just $ testProperty "simpler is consistent" $
       \(Limited _ _ (a :: p)) (Limited _ _ (b :: p)) -> do
         let simpler_ab = runIdentity $ simpler a b
         let simpler_ba = runIdentity $ simpler b a
         simpler_ab    == negOrder simpler_ba
   , Just $ testProperty "simpler is consistent with depthof" $
       \(Limited _ _ (a :: p)) (Limited _ _(b :: p)) -> do
         if runIdentity $ simpler1 a b then
             counterexample (printf "simpler1 a b => depthOf a(%d) <= depthOf b(%d)" (depthOf a) (depthOf b))
                $ depthOf a <= depthOf b
         else
             counterexample (printf "! simpler1 a b => depthOf a(%d) >= depthOf b(%d)" (depthOf a) (depthOf b))
                $ depthOf a >= depthOf b
   ]

runner :: forall a t. (t -> StateT EState Identity a) -> t -> a
runner f ee = do
  let spe   :: Spec   = $never
      seed            = 32
      state :: EState = newEStateWithSeed seed spe
      res             = runIdentity $ flip evalStateT state $ f ee
  res


use_qc :: [Maybe a] -> [Maybe a]
use_qc xs = xs

reduceSimpler :: forall (m :: * -> *) a. Reduce a m => a -> m [a]
reduceSimpler a = do
    rr <- reduce a
    return $ filter (\x -> runIdentity $ simpler1 x a) rr
