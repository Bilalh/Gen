module Gen.IO.Formats where

import Gen.Imports
import Data.Time(formatTime,getCurrentTime)
import Data.Time.Format(defaultTimeLocale)
import Conjure.UI.IO(readModelFromFile)
import Conjure.Language.NameResolution
import Conjure.Language.NameGen ( runNameGen )

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
  start <- ignoreLogs . runNameGen $ resolveNames model
  fromConjure start

readFromJSON :: (MonadFail m, MonadIO m, FromJSON a) => FilePath -> m a
readFromJSON fp = do
  by <- liftIO $ L.readFile fp
  case A.decode by of
    Just r -> return r
    Nothing -> fail $ "Error decoding " <+> pretty fp


readFromJSONMay :: (MonadIO m, FromJSON a)  => FilePath -> m (Maybe a)
readFromJSONMay fp = do
  liftIO $ doesFileExist fp >>= \case
    False -> return Nothing
    True  -> readFromJSON fp


writeToJSON :: (MonadFail m, MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeToJSON fp r = do
  liftIO $ L.writeFile fp (A.encode r)

replaceExtensions :: FilePath -> FilePath -> FilePath
replaceExtensions x y = dropExtensions x <.> y
