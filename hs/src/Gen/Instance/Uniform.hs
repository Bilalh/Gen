{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.Instance.Uniform where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.Method

data Uniform = Uniform
  deriving (Eq, Show, Data, Typeable, Generic)

instance Sampling Uniform where
  doIteration = do
    picked <- randomPoint
    runParamAndStoreQuality picked
    storeDataPoint picked
    return SamplingSuccess
