{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.Instance.NoRacing where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.Method
import Gen.Instance.Point(pointHash,writePoint)

import qualified Data.Aeson as A

data NoRacing = NoRacing
  deriving (Eq, Show, Data, Typeable, Generic)

instance A.FromJSON NoRacing
instance A.ToJSON NoRacing


instance Sampling NoRacing where
  doIteration = do
    randomPoint >>= \case
      Left x -> return $ Left x
      Right point -> do
        (Method MCommon{mOutputDir} _) <- get
        let fp = mOutputDir </> "_params" </> (pointHash point) <.> ".param"
        writePoint point fp

        return $ Right ()
