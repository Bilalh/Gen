module TestGen.Helpers.IO where

import TestGen.Prelude
import Data.Time(formatTime,getCurrentTime)
import System.Locale(defaultTimeLocale)
import Conjure.UI.IO(readModelFromFile)

import qualified Data.ByteString.Lazy as L
import qualified Data.Aeson as A


timestamp :: IO Int
timestamp = do
    epochInt <- (readNote "timestamp" <$> formatTime defaultTimeLocale "%s"
                                      <$> getCurrentTime) :: IO Int
    return epochInt

readSpecFromEssence :: (MonadFail m, MonadIO m) => FilePath -> m Spec
readSpecFromEssence fp = do
  model <- readModelFromFile fp
  fromConjure model

readSpecFromJSON :: (MonadFail m, MonadIO m) => FilePath -> m Spec
readSpecFromJSON fp = do
  by <- liftIO $ L.readFile fp
  case A.decode by of
    Just r -> return r
    Nothing -> fail $ "Error decoding " <+> pretty fp

writeSpecToJSON :: (MonadFail m, MonadIO m) => FilePath -> Spec -> m ()
writeSpecToJSON fp r = do
  liftIO $ L.writeFile fp (A.encode r)
