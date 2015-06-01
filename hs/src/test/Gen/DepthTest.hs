{-# LANGUAGE QuasiQuotes #-}
module Gen.DepthTest ( tests ) where

import Gen.AST.TH
import Gen.Imports
import Gen.TestPrelude
import Gen.Helpers.SizeOf
import Conjure.Language.Definition

typeDepth :: DepthOf a
          => Doc -> Integer -> a -> TestTree
typeDepth name expected ty = testCase (name <+> braces (pretty expected)) $
                               depthOf ty @?= expected

te :: (DepthOf a, Pretty a) => Integer -> a -> TestTree
te i e= typeDepth (pretty e) i e

tests ::  TestTree
tests = testGroup "depthOf"
  [
   testGroup "type"
   [
     typeDepth "Boolean" 0 $ TypeBool
   , typeDepth "Int"     0 $ TypeInt
   , typeDepth "Set"     1 $ (TypeSet TypeBool)
   , typeDepth "MSet"    1 $ (TypeMSet TypeBool)
   , typeDepth "Matrix"  1 $ (TypeMatrix TypeInt TypeBool)
   , typeDepth "Func"    2 $ TypeFunction TypeInt (TypeSet TypeBool)
   , typeDepth "Par"     2 $ TypePartition (TypeMSet TypeBool)
   , typeDepth "Rel"     3 $ TypeRelation [TypeSet TypeInt, TypeMatrix TypeInt (TypeSet TypeBool), TypeInt]
   ]

   ,testGroup "type_gen"
   [
     typeDepth "Matrix Par" 2 $ TypeMatrix TypeInt (TypePartition TypeBool)
   , typeDepth "Matrix 1d"  2 $ TypeMatrix TypeInt (TypeRelation [TypeBool, TypeInt, TypeInt])
   , typeDepth "Matrix 2d"  3
                   $ TypeMatrix TypeInt
                    (TypeMatrix TypeInt (TypeRelation [TypeBool, TypeInt, TypeInt]))
   , typeDepth "Matrix 3d" 4 $ TypeMatrix TypeInt (TypeMatrix TypeInt
                    (TypeMatrix TypeInt (TypeRelation [TypeBool, TypeInt, TypeInt])))
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
   , typeDepth "MSet"          1 $ [essencee| mset(1,2) |]
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
    , te 2 [essencee| (true \/ true) != true |]
    , te 2 [essencee| 10 = 7 \/ 7 ** 10 |]
    , te 2 [essencee| 2 != 2 /\ (true \/ true) |]
    , te 2 [essencee| preImage(function(true --> true), false) |]
    , te 2 [essencee| mset(-5, 4)  |]
    , te 3 [essencee| 1 in mset(-5, 4)  |]
    , te 3 [essencee| toInt(true) in mset(-5, 4)  |]
    , te 4 [essencee| toInt(toInt(true) in mset(-5, 4))  |]
    , te 5 [essencee| toInt(toInt(true) in mset(-5, 4)) = 9 |]
    , te 2 [essencee| or([true]) |]
    , te 1 [essencee| or([false,true]) |]
    , te 2 [essencee| or([false,true,false])|]
    , te 3 [essencee| or([true/\false]) |]
    , te 2 [essencee| or([true/\false,true/\false]) |]
    , te 3 [essencee| or([true/\false,true/\false,true/\false]) |]
   ]

  ,testGroup "Breaking"
   [
     te 3 [essencee| 10 = 7 \/ -2 = 0 |]
   , te 3 [essencee| true \/ 7 % 10 = 3 |]
   , te 2 [essencee|  (relation() : `relation of (matrix indexed by [int] of bool)`)  |]
   , te 2 [essencee| relation( tuple([true, true; int(1..2)] )) |]
   , te 2 [essencee|  ({} : `set of (matrix indexed by [int] of bool)`)  |]
   , te 2 [essencee| { [true, true; int(1..2)] } |]
   , te 2 [essencee| |mset(1)| |]
   , te 2 $ do
       let a = Single "l1"
       let b = EVar (Var "l1" TypeInt)
       [essencee|[ &b | &a : int(1..5, 0)] |]
   ]

  ]
