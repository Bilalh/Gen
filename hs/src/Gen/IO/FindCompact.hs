{-# LANGUAGE DeriveGeneric, TupleSections #-}
module Gen.IO.FindCompact(findCompact, lookupCompact) where

import Crypto.Hash
import Gen.Imports     hiding (hash)
import System.FilePath (replaceFileName, takeBaseName)

import qualified Data.ByteString.Char8 as C

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
  compact_hash <- hashFileStrict compactFP
  hashes <- mapM hashFileStrict to_check
  case [  path | (h,path) <- zip hashes to_check, h == compact_hash ] of
    []  -> return Nothing
    [x] -> return (Just x)
    _   -> error "Multiple Models match compact"

hashFileStrict :: MonadIO m => FilePath -> m (Digest MD5)
hashFileStrict fp = liftIO $ do
  content <- C.readFile fp
  let slns = C.concat $ [ x | x <- C.lines content
                        -- eprime choices
                        , not $ "$"   `C.isPrefixOf` x]
  return $ hash slns
