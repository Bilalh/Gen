{-# LANGUAGE QuasiQuotes #-}
module Instance.PointTest ( tests ) where

import Gen.Imports
import Gen.Instance.Point
import Gen.Instance.Data
import Gen.TestPrelude
import Conjure.Language.Definition

distanceTest :: forall a. (Pretty a, Distance a)
             => Doc -> Integer -> a -> a -> TestTree
distanceTest name expected a b = testCase (name  ) $
                                   distance a b @?= expected


tests ::  TestTree
tests = testGroup "PointTests"
  [
   testGroup "known Constants"
   [
     let ma = ConstantInt 50
         mi = ConstantInt 1
     in distanceTest "langford n" 49 ma mi
   , let ma = ConstantAbstract $ AbsLitFunction
                [ (ConstantInt i, ConstantInt 100) | i  <- [1..100] ]
         mi = ConstantAbstract $ AbsLitFunction
                [ (ConstantInt i, ConstantInt 1) | i  <- [1..100] ]
     in distanceTest "knapsack Func" 990 ma mi
   ]
   , testGroup "known Points"
   [
     let ma = Point [("k", ConstantInt 10), ("n", ConstantInt 50)]
         mi = Point [("k", ConstantInt 2),  ("n", ConstantInt 1)]
     in distanceTest "langford point" 49 ma mi
   , let ma = Point [ ("n", ConstantInt 100)
                    , ("totalWeight", ConstantInt 1000)
                    , ("weights",  ConstantAbstract $ AbsLitFunction
                       [ (ConstantInt i, ConstantInt 100) | i  <- [1..100] ])
                    , ("values",  ConstantAbstract $ AbsLitFunction
                       [ (ConstantInt i, ConstantInt 100) | i  <- [1..100] ])
                    ]
         mi = Point [ ("n", ConstantInt 1)
                    , ("totalWeight", ConstantInt 1)
                    , ("weights",  ConstantAbstract $ AbsLitFunction
                       [ (ConstantInt i, ConstantInt 1) | i  <- [1..100] ])
                    , ("values",  ConstantAbstract $ AbsLitFunction
                       [ (ConstantInt i, ConstantInt 1) | i  <- [1..100] ])
                    ]
     in distanceTest "knapsack point" 1722 ma mi
   ]

  ]
