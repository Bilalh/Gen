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

-- 190 elements
sonnet_demad_max :: Constant
sonnet_demad_max = ConstantAbstract
  (AbsLitSet
     [ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 2]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 3]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 4]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 5]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 6]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 7]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 8]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 9]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 10]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 11]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 12]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 13]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 14]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 15]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 16]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 1, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 3]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 4]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 5]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 6]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 7]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 8]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 9]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 10]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 11]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 12]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 13]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 14]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 15]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 16]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 2, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 4]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 5]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 6]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 7]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 8]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 9]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 10]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 11]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 12]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 13]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 14]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 15]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 16]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 3, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 5]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 6]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 7]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 8]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 9]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 10]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 11]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 12]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 13]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 14]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 15]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 16]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 4, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 5, ConstantInt 6]),
      ConstantAbstract (AbsLitSet [ConstantInt 5, ConstantInt 7]),
      ConstantAbstract (AbsLitSet [ConstantInt 5, ConstantInt 8]),
      ConstantAbstract (AbsLitSet [ConstantInt 5, ConstantInt 9]),
      ConstantAbstract (AbsLitSet [ConstantInt 5, ConstantInt 10]),
      ConstantAbstract (AbsLitSet [ConstantInt 5, ConstantInt 11]),
      ConstantAbstract (AbsLitSet [ConstantInt 5, ConstantInt 12]),
      ConstantAbstract (AbsLitSet [ConstantInt 5, ConstantInt 13]),
      ConstantAbstract (AbsLitSet [ConstantInt 5, ConstantInt 14]),
      ConstantAbstract (AbsLitSet [ConstantInt 5, ConstantInt 15]),
      ConstantAbstract (AbsLitSet [ConstantInt 5, ConstantInt 16]),
      ConstantAbstract (AbsLitSet [ConstantInt 5, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 5, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 5, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 5, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 6, ConstantInt 7]),
      ConstantAbstract (AbsLitSet [ConstantInt 6, ConstantInt 8]),
      ConstantAbstract (AbsLitSet [ConstantInt 6, ConstantInt 9]),
      ConstantAbstract (AbsLitSet [ConstantInt 6, ConstantInt 10]),
      ConstantAbstract (AbsLitSet [ConstantInt 6, ConstantInt 11]),
      ConstantAbstract (AbsLitSet [ConstantInt 6, ConstantInt 12]),
      ConstantAbstract (AbsLitSet [ConstantInt 6, ConstantInt 13]),
      ConstantAbstract (AbsLitSet [ConstantInt 6, ConstantInt 14]),
      ConstantAbstract (AbsLitSet [ConstantInt 6, ConstantInt 15]),
      ConstantAbstract (AbsLitSet [ConstantInt 6, ConstantInt 16]),
      ConstantAbstract (AbsLitSet [ConstantInt 6, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 6, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 6, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 6, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 7, ConstantInt 8]),
      ConstantAbstract (AbsLitSet [ConstantInt 7, ConstantInt 9]),
      ConstantAbstract (AbsLitSet [ConstantInt 7, ConstantInt 10]),
      ConstantAbstract (AbsLitSet [ConstantInt 7, ConstantInt 11]),
      ConstantAbstract (AbsLitSet [ConstantInt 7, ConstantInt 12]),
      ConstantAbstract (AbsLitSet [ConstantInt 7, ConstantInt 13]),
      ConstantAbstract (AbsLitSet [ConstantInt 7, ConstantInt 14]),
      ConstantAbstract (AbsLitSet [ConstantInt 7, ConstantInt 15]),
      ConstantAbstract (AbsLitSet [ConstantInt 7, ConstantInt 16]),
      ConstantAbstract (AbsLitSet [ConstantInt 7, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 7, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 7, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 7, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 8, ConstantInt 9]),
      ConstantAbstract (AbsLitSet [ConstantInt 8, ConstantInt 10]),
      ConstantAbstract (AbsLitSet [ConstantInt 8, ConstantInt 11]),
      ConstantAbstract (AbsLitSet [ConstantInt 8, ConstantInt 12]),
      ConstantAbstract (AbsLitSet [ConstantInt 8, ConstantInt 13]),
      ConstantAbstract (AbsLitSet [ConstantInt 8, ConstantInt 14]),
      ConstantAbstract (AbsLitSet [ConstantInt 8, ConstantInt 15]),
      ConstantAbstract (AbsLitSet [ConstantInt 8, ConstantInt 16]),
      ConstantAbstract (AbsLitSet [ConstantInt 8, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 8, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 8, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 8, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 9, ConstantInt 10]),
      ConstantAbstract (AbsLitSet [ConstantInt 9, ConstantInt 11]),
      ConstantAbstract (AbsLitSet [ConstantInt 9, ConstantInt 12]),
      ConstantAbstract (AbsLitSet [ConstantInt 9, ConstantInt 13]),
      ConstantAbstract (AbsLitSet [ConstantInt 9, ConstantInt 14]),
      ConstantAbstract (AbsLitSet [ConstantInt 9, ConstantInt 15]),
      ConstantAbstract (AbsLitSet [ConstantInt 9, ConstantInt 16]),
      ConstantAbstract (AbsLitSet [ConstantInt 9, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 9, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 9, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 9, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 10, ConstantInt 11]),
      ConstantAbstract (AbsLitSet [ConstantInt 10, ConstantInt 12]),
      ConstantAbstract (AbsLitSet [ConstantInt 10, ConstantInt 13]),
      ConstantAbstract (AbsLitSet [ConstantInt 10, ConstantInt 14]),
      ConstantAbstract (AbsLitSet [ConstantInt 10, ConstantInt 15]),
      ConstantAbstract (AbsLitSet [ConstantInt 10, ConstantInt 16]),
      ConstantAbstract (AbsLitSet [ConstantInt 10, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 10, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 10, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 10, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 11, ConstantInt 12]),
      ConstantAbstract (AbsLitSet [ConstantInt 11, ConstantInt 13]),
      ConstantAbstract (AbsLitSet [ConstantInt 11, ConstantInt 14]),
      ConstantAbstract (AbsLitSet [ConstantInt 11, ConstantInt 15]),
      ConstantAbstract (AbsLitSet [ConstantInt 11, ConstantInt 16]),
      ConstantAbstract (AbsLitSet [ConstantInt 11, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 11, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 11, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 11, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 12, ConstantInt 13]),
      ConstantAbstract (AbsLitSet [ConstantInt 12, ConstantInt 14]),
      ConstantAbstract (AbsLitSet [ConstantInt 12, ConstantInt 15]),
      ConstantAbstract (AbsLitSet [ConstantInt 12, ConstantInt 16]),
      ConstantAbstract (AbsLitSet [ConstantInt 12, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 12, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 12, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 12, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 13, ConstantInt 14]),
      ConstantAbstract (AbsLitSet [ConstantInt 13, ConstantInt 15]),
      ConstantAbstract (AbsLitSet [ConstantInt 13, ConstantInt 16]),
      ConstantAbstract (AbsLitSet [ConstantInt 13, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 13, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 13, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 13, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 14, ConstantInt 15]),
      ConstantAbstract (AbsLitSet [ConstantInt 14, ConstantInt 16]),
      ConstantAbstract (AbsLitSet [ConstantInt 14, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 14, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 14, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 14, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 15, ConstantInt 16]),
      ConstantAbstract (AbsLitSet [ConstantInt 15, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 15, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 15, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 15, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 16, ConstantInt 17]),
      ConstantAbstract (AbsLitSet [ConstantInt 16, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 16, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 16, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 17, ConstantInt 18]),
      ConstantAbstract (AbsLitSet [ConstantInt 17, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 17, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 18, ConstantInt 19]),
      ConstantAbstract (AbsLitSet [ConstantInt 18, ConstantInt 20]),
      ConstantAbstract (AbsLitSet [ConstantInt 19, ConstantInt 20])])
