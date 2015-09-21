{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Instance.Uniform where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.Method

instance Sampling Uniform where
  doIteration = do
    picked <- randomPoint
    createRunParamAndStoreQuality picked
    storeDataPoint picked
    return SamplingSuccess
