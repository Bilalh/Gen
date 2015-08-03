module Gen.IO.Formats where


import Conjure.Language.Definition
import Conjure.Language.NameResolution
import Conjure.Language.Parser
import Conjure.Language.Pretty
import Conjure.UI.IO                   (readModelFromFile)
import Conjure.UserError
import Data.Time                       (formatTime, getCurrentTime)
import Data.Time.Format                (defaultTimeLocale)
import Gen.Imports
import System.Directory                (copyFile)
import System.Posix                    (getFileStatus)
import System.Posix.Files              (fileSize)

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T


timestamp :: MonadIO m => m Int
timestamp = do
    epochInt <- liftIO $ (readNote "timestamp" <$> formatTime defaultTimeLocale "%s"
                                               <$> getCurrentTime)
    return epochInt

readSpecFromEssence :: (MonadFail m, MonadUserError m, MonadIO m)
                    => FilePath -> m Spec
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


copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory from to = do
  createDirectoryIfMissing True to
  fps <- (getDirectoryContents from)
  forM_ (filter (`notElem` [".", ".."])  fps) $ \f -> do
    doesDirectoryExist f >>= \case
      True  -> return ()
      False -> copyFile (from </> f) (to </> f)


getFileSize :: FilePath -> IO Integer
getFileSize path = getFileStatus
                   path >>= \s -> return $ fromIntegral $ fileSize s


readEprimeAsEssence :: (MonadFail m, MonadIO m) => FilePath -> m Model
readEprimeAsEssence fp= do
  pa <- liftIO $ pairWithContents fp
  m <- readModel2 discardConjureJSON pa
  return m{mLanguage=def}

  where
  discardConjureJSON :: Text -> Text
  discardConjureJSON = discardAfter "$ Conjure's"
    where discardAfter t = fst . T.breakOn t


readModel2 :: (MonadFail m) => (Text -> Text) -> (FilePath, Text) -> m Model
readModel2 preprocess (fp, con) =
  case runLexerAndParser parseModel fp (preprocess con) of
    Left  e -> fail e
    Right x -> return x
