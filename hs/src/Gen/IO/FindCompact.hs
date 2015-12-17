{-# LANGUAGE DeriveGeneric, TupleSections #-}
module Gen.IO.FindCompact(findCompact, lookupCompact) where

import Crypto.Hash
import Gen.Imports     hiding (hash)
import System.FilePath (replaceFileName, takeBaseName)
import Conjure.UI.NormaliseQuantified(normaliseQuantifiedVariables)
import Conjure.UI.IO(readModel)
import Conjure.Language.Pretty

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T

type Dir = FilePath

-- the code in gen main should use this
lookupCompact :: MonadIO m => Dir -> FilePath -> m (Maybe FilePath)
lookupCompact models_path essence_path  = do

  let compactPaths = [ (models_path ++ "-compact")
                     , replaceFileName essence_path (takeBaseName essence_path ++ "_" ++ "df-compact")
                     ]
  cExists <- liftIO$ filterM doesDirectoryExist compactPaths

  compactFirst <- case cExists of
    [] -> do
      liftIO $ print . pretty $  "no compact found, at"  <+> vcat (map pretty compactPaths)
      return Nothing
    (path:_)  -> do
      liftIO $  print . pretty $ "checking for compact in: " <+> pretty path
      cs <- liftIO $ getAllFilesWithSuffix ".eprime" path
      catMaybes <$> mapM ((flip findCompact) models_path) cs  >>= \case
        []   -> return Nothing
        [x]  -> do
          liftIO $  print . pretty $ "compact_twin is" <+> pretty x
          return $ Just $ (takeBaseName x)
        (x:_) -> do
          liftIO $  print . pretty $ "Picking first compact_twin" <+> pretty x
          return $ Just $ (takeBaseName x)

  return compactFirst


findCompact :: MonadIO m => FilePath -> Dir -> m (Maybe FilePath)
findCompact compactFP dir = do
  to_check <- liftIO $ getAllFilesWithSuffix ".eprime" dir
  compact_hash <- hashModel compactFP
  hashes <- mapM hashModel to_check
  case [  path | (h,path) <- zip hashes to_check, h == compact_hash ] of
    []  -> return Nothing
    [x] -> return (Just x)
    _   -> error "Multiple Models match compact"

hashModel :: MonadIO m => FilePath -> m (Digest MD5)
hashModel fp = do
  pair <- liftIO $ pairWithContents fp
  m <- liftIO $ readModel stripComments pair
  let normalised = normaliseQuantifiedVariables m
  let encoded = C.pack $ renderNormal normalised
  return $ hash encoded


stripComments :: Text -> Text
stripComments = T.unlines . map (T.takeWhile (/= '$')) . T.lines
