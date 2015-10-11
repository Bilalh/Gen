module Instance.PointTest ( tests ) where

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

   , let ma = Point [ ("nnodes", ConstantInt 100)
                    , ("maxl", ConstantInt 100)
                    , ("minl", ConstantInt 100)
                    , ("n_courses", ConstantInt 100)
                    , ("maxc", ConstantInt 100)
                    , ("minc", ConstantInt 100)
                    , ("n_credits", ConstantInt 100)
                    , ("prereq", relation_prereq_max)
                    , ("credits",  ConstantAbstract $ AbsLitFunction
                       [ (ConstantInt i, ConstantInt 100) | i  <- [1..100] ])
                    ]
         mi = Point [ ("nnodes", ConstantInt 1)
                    , ("maxl", ConstantInt 1)
                    , ("minl", ConstantInt 1)
                    , ("n_courses", ConstantInt 1)
                    , ("maxc", ConstantInt 1)
                    , ("minc", ConstantInt 1)
                    , ("n_credits", ConstantInt 1)
                    , ("prereq", ConstantAbstract (AbsLitRelation []))
                    , ("credits",  ConstantAbstract $ AbsLitFunction
                       [ (ConstantInt i, ConstantInt 1) | i  <- [1..100] ])
                    ]
     in distanceTest "bacp point max distance" 1028 ma mi


   ]

  ]
