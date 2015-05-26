{-# LANGUAGE FlexibleInstances, QuasiQuotes #-}
module Gen.OlderTests ( tests ) where

import Gen.Arbitrary.Generators
import Gen.Imports
import Gen.TestPrelude
import Gen.Reduce.Simpler
import Test.Tasty.QuickCheck    as QC
import Gen.Helpers.SizeOf
import Gen.Arbitrary.Data(SS(..))

_use_qc :: [Maybe a] -> [Maybe a]
_use_qc xs = xs

tests :: TestTree
tests = testGroup "simpler"
  [testGroup "type_QC" $
   catMaybes $ _use_qc [
     Just $ QC.testProperty "type is equal to self" $
       \(AType a) ->  (runIdentity $ simpler a a) == EQ
   , no $ QC.testProperty "atype and depthOf argee" $
       \(BType ty gen_depth) ->  depthOf ty == fromIntegral gen_depth
   , Just $ QC.testProperty "simpler is consistent" $
       \(SType a) (SType b) -> do
         let simpler_ab = runIdentity $ simpler a b
         let simpler_ba = runIdentity $ simpler b a
         simpler_ab    == negOrder simpler_ba
   ]
  ]


newtype AType =  AType Type
    deriving (Show,Eq)

instance Arbitrary AType where
    arbitrary = flip evalStateT def{depth_=3} $ fmap AType atype

data BType =  BType Type Int
    deriving (Show,Eq)

instance Arbitrary BType where
    arbitrary = sized $ \s -> do
      i <- choose (1,  (max 0 (min s 3)) )
      res <- flip evalStateT def{depth_=i} atype
      return $ BType res i

data SType =  SType Type
    deriving (Show,Eq)

instance Arbitrary SType where
  arbitrary = sized $ \s -> do
    i <- choose (1,  (max 0 (min s 3)) )
    SType <$> flip evalStateT def{depth_=i} atype
