{-# LANGUAGE QuasiQuotes #-}
module Gen.DepthTest ( tests ) where

import Gen.Prelude
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Gen.AST.TH


typeDepth :: DepthOf a
          => String -> Integer -> a -> TestTree
typeDepth name expected ty = testCase (name ++ " {" ++ show expected ++ "}") $ depthOf ty @?= expected

te :: (DepthOf a, Pretty a) => Integer -> a -> TestTree
te i e= typeDepth (show $ pretty e) i e

tests ::  TestTree
tests = testGroup "depthOf"
  [
   testGroup "type"
   [
     typeDepth "Boolean" 0 $ TBool
   , typeDepth "Int"     0 $ TInt
   , typeDepth "Set"     1 $ (TSet TBool)
   , typeDepth "MSet"    1 $ (TMSet TBool)
   , typeDepth "Matrix"  1 $ (TMatix TBool)
   , typeDepth "Func"    2 $ TFunc TInt (TSet TBool)
   , typeDepth "Par"     2 $ TPar (TMSet TBool)
   , typeDepth "Rel"     3 $ TRel [TSet TInt, TMatix (TSet TBool), TInt]
   ]

   ,testGroup "type_gen"
   [
     typeDepth "Par"     2 $ (TMatix (TPar TBool))
   ]

  ,testGroup "Constants"
   [
     typeDepth "Boolean" 0 $ [essencee| true |]
   , typeDepth "Int"     0 $ [essencee| 1 |]
   ]

  ,testGroup "Literals"
   [
     typeDepth "Set"           1 $ [essencee| {1,2,4} |]
   , typeDepth "Set Empty"     1 $ [essencee| {} |]
   , typeDepth "MSet Empty"    1 $ [essencee| mset() |]
   , typeDepth "Matrix"        1 $ [essencee| [1] |]
   , typeDepth "Matrix Empty"  1 $ [essencee| [] |]
   , typeDepth "Func"          1 $ [essencee| function( 1 --> 3 ) |]
   , typeDepth "Func Empty"    1 $ [essencee| function(  ) |]
   , typeDepth "Par"           1 $ [essencee| partition( {3} ) |]
   , typeDepth "Rel"           1 $ [essencee| relation( tuple(3), tuple(2,3,4,4)  ) |]
   ]

  ,testGroup "Expr_gen"
   [
      te 0 [essencee| false |]
    , te 1 [essencee| false \/ false |]
    , te 1 [essencee| false != true |]
    , te 2 [essencee| 4 = -5 |]
    , te 2 [essencee| function(0 --> 3) = function(1 --> 7, 10 --> 6) |]
    , te 2 [essencee| partition({4}) != partition({7}, {3}) |]
    , te 2 [essencee| {true} = {true,true} |]
    , te 3 [essencee| (true \/ true) != true |]
    , te 2 [essencee| 10 = 7 \/ 7 ** 10 |]
    , te 2 [essencee| 2 != 2 /\ (true \/ true) |]
    , te 2 [essencee| preImage(function(true --> true), false) |]
    , te 3 [essencee| toInt(toInt(true) in mset(-5, 4)) = 9 |]
   ]

  ]
