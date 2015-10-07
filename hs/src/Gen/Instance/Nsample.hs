{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.Instance.Nsample where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.Method

import qualified Data.Aeson as A

data Nsample = Nsample
  deriving (Eq, Show, Data, Typeable, Generic)

instance A.FromJSON Nsample
instance A.ToJSON Nsample

instance Sampling Nsample where
  doIteration = do
    randomPoint >>= \case
      Left x -> return $ Left x
      Right picked -> do
        runParamAndStoreQuality picked >>= \case
          Left err -> return $ Left err
          Right{}  -> do
            storeDataPoint picked
            return $ Right ()

