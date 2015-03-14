{-# LANGUAGE QuasiQuotes #-}
module Gen.DepthTest ( tests ) where

import Gen.Prelude
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Gen.AST.TH


typeDepth :: DepthOf a
          => String -> Integer -> a -> TestTree
typeDepth name expected ty = testCase (name ++ " (" ++ show expected ++ ")") $ depthOf ty @?= expected

tests :: TestTree
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

  ]
