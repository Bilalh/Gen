module Gen.Helpers.IO where

import Gen.Prelude
import Data.Time(formatTime,getCurrentTime)
import System.Locale(defaultTimeLocale)
import Conjure.UI.IO(readModelFromFile)
import System.FilePath((<.>))

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

readFromJSON :: (MonadFail m, MonadIO m, FromJSON a) => FilePath -> m a
readFromJSON fp = do
  by <- liftIO $ L.readFile fp
  case A.decode by of
    Just r -> return r
    Nothing -> fail $ "Error decoding " <+> pretty fp


readFromJSONMay :: FromJSON a => FilePath -> IO (Maybe a)
readFromJSONMay fp = do
  doesFileExist fp >>= \case
    False -> return Nothing
    True  -> readFromJSON fp


writeToJSON :: (MonadFail m, MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeToJSON fp r = do
  liftIO $ L.writeFile fp (A.encode r)

replaceExtensions :: FilePath -> FilePath -> FilePath
replaceExtensions x y = dropExtensions x <.> y
