module Instance.PointTest ( tests ) where
-- Test the distance between two, Constants and two Points

import Conjure.Language.Definition
import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.Point
import Gen.TestPrelude
import Instance.PointTestData

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
     in distanceTest "langford point max difference" 49 ma mi
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
     in distanceTest "knapsack point max difference" 1722 ma mi
   , let ma = Point [ ("nnodes", ConstantInt 20)
                    , ("nrings", ConstantInt 20)
                    , ("capacity", ConstantInt 20)
                    , ("demand", ConstantAbstract (AbsLitSet
                          [ ConstantAbstract (AbsLitSet [ConstantInt 1,ConstantInt 2])
                          , ConstantAbstract (AbsLitSet [ConstantInt 3,ConstantInt 4])]))
                    ]
         mi = Point [ ("nnodes", ConstantInt 1)
                    , ("nrings", ConstantInt 1)
                    , ("capacity", ConstantInt 1)
                    , ("demand", ConstantAbstract (AbsLitSet
                          [ ConstantAbstract (AbsLitSet [ConstantInt 1,ConstantInt 2])
                          , ConstantAbstract (AbsLitSet [ConstantInt 3,ConstantInt 4])]))
                    ]
     in distanceTest "sonet like point" 32 ma mi
   , let ma = Point [ ("nnodes", ConstantInt 20)
                    , ("nrings", ConstantInt 20)
                    , ("capacity", ConstantInt 20)
                    , ("demand", sonnet_demad_max)
                    ]
         mi = Point [ ("nnodes", ConstantInt 1)
                    , ("nrings", ConstantInt 1)
                    , ("capacity", ConstantInt 1)
                    , ("demand", ConstantAbstract (AbsLitSet []))
                    ]
     in distanceTest "sonet point max distance" 35 ma mi

   ]

  ]
