{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.Instance.Undirected where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.Method

import qualified Data.Aeson as A

data Undirected = Undirected
  deriving (Eq, Show, Data, Typeable, Generic)

instance A.FromJSON Undirected
instance A.ToJSON Undirected


instance Sampling Undirected where
  doIteration = do
    randomPoint >>= \case
      Left x -> return $ Left x
      Right picked -> do
        runParamAndStoreQuality picked >>= \case
          Left err -> return $ Left err
          Right{}  -> do
            storeDataPoint picked
            return $ Right ()

