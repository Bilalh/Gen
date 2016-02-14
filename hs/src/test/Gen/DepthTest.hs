{-# LANGUAGE QuasiQuotes #-}
module Gen.DepthTest ( tests ) where

import Gen.AST.TH
import Gen.Imports
import Gen.TestPrelude
import Gen.Helpers.SizeOf
import Conjure.Language.Definition

depthTest :: DepthOf a
          => Doc -> Integer -> a -> TestTree
depthTest name expected ty = testCase ( pretty expected <+> "⇒" <+> name) $
                               depthOf ty @?= expected


de :: Integer -> Domain () Expr -> TestTree
de i e= depthTest (pretty e) i e

te :: (DepthOf a, Pretty a) => Integer -> a -> TestTree
te i e= depthTest (pretty e) i e

tests ::  TestTree
tests = testGroup "depthOf"
  [
   testGroup "type"
   [
     depthTest "Boolean" 0 $ TypeBool
   , depthTest "Int"     0 $ TypeInt
   , depthTest "Set"     1 $ (TypeSet TypeBool)
   , depthTest "MSet"    1 $ (TypeMSet TypeBool)
   , depthTest "Matrix"  1 $ (TypeMatrix TypeInt TypeBool)
   , depthTest "Func"    2 $ TypeFunction TypeInt (TypeSet TypeBool)
   , depthTest "Par"     2 $ TypePartition (TypeMSet TypeBool)
   , depthTest "Rel"     3 $ TypeRelation [TypeSet TypeInt, TypeMatrix TypeInt (TypeSet TypeBool), TypeInt]
   ]

   ,testGroup "Domain"
   [
    de 0 $ [domainn| bool |]
   ,de 0 $ [domainn| int(0) |]
   ,de 1 $ [domainn| set of bool |]
   ,de 1 $ [domainn| mset of bool |]
   ,de 1 $ [domainn| matrix indexed by [int(1..3)] of bool |]
   ,de 2 $ [domainn| function int --> set of bool |]
   ,de 2 $ [domainn| partition from set of bool |]
   ,de 4 $ [domainn| function (total, injective) int(1..4*|{4,2}| ) --> bool |]
   ]

   ,testGroup "type_gen"
   [
     depthTest "Matrix Par" 2 $ TypeMatrix TypeInt (TypePartition TypeBool)
   , depthTest "Matrix 1d"  2 $ TypeMatrix TypeInt (TypeRelation [TypeBool, TypeInt, TypeInt])
   , depthTest "Matrix 2d"  3
                   $ TypeMatrix TypeInt
                    (TypeMatrix TypeInt (TypeRelation [TypeBool, TypeInt, TypeInt]))
   , depthTest "Matrix 3d" 4 $ TypeMatrix TypeInt (TypeMatrix TypeInt
                    (TypeMatrix TypeInt (TypeRelation [TypeBool, TypeInt, TypeInt])))
   ]

  ,testGroup "Constants"
   [
     depthTest "Boolean" 0 $ [essencee| true |]
   , depthTest "Int"     0 $ [essencee| 1 |]
   ]

  ,testGroup "Literals"
   [
     depthTest "Set"           1 $ [essencee| {1,2,4} |]
   , depthTest "Set Empty"     1 $ [essencee| {} |]
   , depthTest "MSet Empty"    1 $ [essencee| mset() |]
   , depthTest "MSet"          1 $ [essencee| mset(1,2) |]
   , depthTest "Matrix"        1 $ [essencee| [1] |]
   , depthTest "Matrix Empty"  1 $ [essencee| [] |]
   , depthTest "Func"          1 $ [essencee| function( 1 --> 3 ) |]
   , depthTest "Func Empty"    1 $ [essencee| function(  ) |]
   , depthTest "Par"           1 $ [essencee| partition( {3} ) |]
   , depthTest "Rel"           1 $ [essencee| relation( tuple(3), tuple(2,3,4,4)  ) |]
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
   , te 1 $ do
       let a = Single "l1"
       let b = EVar (Var "l1" TypeInt)
       [essencee|[ &b | &a : int(1..5, 0)] |]
   , te 2 $ do
       let a = Single "l1"
       let b = EVar (Var "l1" TypeInt)
       [essencee|[ &b < 3 | &a : int(1..5, 0)] |]
   , te 2 $ do
       let a = Single "l1"
       let b = EVar (Var "l1" $ TypeSet TypeInt)
       [essencee|[ &b | &a : set of int(1..5, 0)] |]
   ]



  ,testGroup "CompSR136_Depth" $ do
     let m1   = Single "m1"
     let q5   = Single "q5"
     let t1   = Single "t1"

     let _m1   = EVar $ Var "m1" (TypeInt)
     let _q5   = EVar $ Var "q5" (TypeInt)


     [
        de 0 [domainn| int(1..4) |]
      , de 1 [domainn| int(1..4+1) |]
      , de 2 [domainn| int(1..4+1*3) |]

      , te 2 [essencee| [1,2][0] |]
      , de 2 [domainn| int(1..[1,2][0])  |]

      , te 2 [essencee| and([true]) |]
      , te 2 [essencee| and([true
                      | &m1 : int(1..3)]) |]

      , te 2 [essencee| and([true
                      | &m1 : int(1..3)
                      , &q5 : int(1..4)]) |]

      , te 4 [essencee| and([true
                 | &m1 : int(1..3)
                 , &q5 : int(1..4)
                 , &t1 : int(0..[3; int(1..1)][&_q5])]) |]
      , te 7 [essencee| and([true
                 | &m1 : int(1..3)
                 , &q5 : int(1..4)
                 , &t1 : int(0..[[5, 5; int(1..2)], [5, 5; int(1..2)],
                                [5, 5; int(1..2)]; int(1..3)][&_m1, &_q5]
                               - 1)])|]

      ]

  ,testGroup "CompSR136~Original_Depth" $ do
     let b2  = Single "b2"
     let t1  = Single "t1"
     let b2_ = EVar $ Var "b2" (TypeInt)

     [
        te 2 [essencee| and([true |             &t1 : int(0..&b2_)])     |]
      , te 3 [essencee| and([true | &b2 <- [1], &t1 : int(0..&b2_)])     |]
      , te 3 [essencee| and([true |             &t1 : int(0..&b2_-1)])   |]
      , te 3 [essencee| and([true | &b2 <- [1], &t1 : int(0..&b2_-1)])   |]
      , te 4 [essencee| and([true | &b2 <- [1], &t1 : int(0..&b2_-1*2)]) |]

      ]


  ]
