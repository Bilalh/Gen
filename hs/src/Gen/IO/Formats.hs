module Gen.IO.Formats where

import Conjure.Language.Definition
import Conjure.Language.NameResolution
import Conjure.Language.Parser
import Conjure.Language.Pretty
import Conjure.Process.LettingsForComplexInDoms
import Conjure.UI.IO                            (readModelFromFile)
import Conjure.UI.TypeCheck                     (typeCheckModel)
import Conjure.UserError
import Data.Time                                (formatTime, getCurrentTime)
import Data.Time.Format                         (defaultTimeLocale)
import Gen.Imports
import System.Directory                         (copyFile, getHomeDirectory)
import System.FilePath                          (takeExtensions)
import System.Posix                             (getFileStatus)
import System.Posix.Files                       (fileSize)

import qualified Control.Exception        as Exc
import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode.Pretty as AA
import qualified Data.ByteString.Lazy     as L
import qualified Data.Text                as T
import qualified System.Exit              as Exc


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
  liftIO $ L.writeFile fp (AA.encodePretty'
    AA.Config{ AA.confIndent = 4, AA.confCompare = compare}  r)

replaceExtensions :: FilePath -> FilePath -> FilePath
replaceExtensions x y = dropExtensions x <.> y

filesWithExtension :: String -> FilePath -> IO [FilePath]
filesWithExtension suffix fp = filter ((==) suffix . takeExtensions) <$> getAllFiles fp

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


readEprimeAsSpec :: MonadIO m => FilePath -> m (Maybe Spec)
readEprimeAsSpec fp = do
  model <- readEprimeAsEssence fp
  case model of
    Nothing -> return Nothing
    Just x  ->  case fromConjure x of
        Left{}    -> return Nothing
        (Right y) -> return $ Just y

readEprimeAsEssence :: MonadIO m => FilePath -> m (Maybe Model)
readEprimeAsEssence fp= do
  liftIO $ (flip Exc.catches) handlers $ do
    pa <- liftIO $ pairWithContents fp
    m <- readModel2 discardConjureJSON pa
    named <- ignoreLogs . runNameGen $ resolveNames $ m{mLanguage=def}
    x <- ignoreLogs . runNameGen . typeCheckModel $ named
    return $ Just x

  where

  handlers = [ Exc.Handler handler1  -- MonadUserError
             -- , Exc.Handler handler2  -- File access
             , Exc.Handler handler3  -- MonadFail
             ]

  handler1 :: Exc.ExitCode -> IO (Maybe Model)
  handler1 _ = return Nothing

  -- handler2 :: Exc.IOException -> IO (Maybe Model)
  -- handler2 _ = return Nothing

  handler3 :: Exc.ErrorCall -> IO (Maybe Model)
  handler3 _ = return Nothing

  discardConjureJSON :: Text -> Text
  discardConjureJSON = discardAfter "$ Conjure's"
    where discardAfter t = fst . T.breakOn t


readModel2 :: (MonadFail m) => (Text -> Text) -> (FilePath, Text) -> m Model
readModel2 preprocess (fp, con) =
  case runLexerAndParser parseModel fp (preprocess con) of
    Left  e -> fail e
    Right x -> return x


allGivensOfEssence :: FilePath -> IO [(Text,Domain () Expression)]
allGivensOfEssence fp = do
  essenceM <- readModelFromFile fp >>= inlineLettingDomainsForDecls
  let givens = [ (nm, dom) | Declaration (FindOrGiven Given (Name nm) dom)
                          <- mStatements essenceM ]
  return givens


getFullPath :: FilePath -> IO FilePath
getFullPath s = do
    homeDir <- getHomeDirectory
    return $ case s of
        "~"             -> homeDir
        ('~' : '/' : t) -> homeDir </> t
        _               -> s
