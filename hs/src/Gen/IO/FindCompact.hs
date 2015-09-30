{-# LANGUAGE TupleSections, DeriveGeneric #-}
module Gen.IO.FindCompact(findCompact) where

import Gen.Imports
import Data.Digest.Pure.MD5

import qualified Data.ByteString.Char8 as C

type Dir = FilePath
findCompact :: MonadIO m => FilePath -> Dir -> m (Maybe FilePath)
findCompact compactFP dir = do
  to_check <- liftIO $ allFilesWithSuffix ".eprime" dir
  compact_hash <- hashFileStrict compactFP
  hashes <- mapM hashFileStrict to_check
  case [  path | (h,path) <- zip hashes to_check, h == compact_hash ] of
    []  -> return Nothing
    [x] -> return (Just x)
    _   -> error "Multiple Models match compact"

hashFileStrict :: MonadIO m => FilePath -> m MD5Digest
hashFileStrict fp = liftIO $ do
  content <- C.readFile fp
  let slns = C.concat $ [ x | x <- C.lines content
                        -- eprime choices
                        , not $ "$"   `C.isPrefixOf` x]
  return $ hash' slns
