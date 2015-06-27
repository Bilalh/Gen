{-# LANGUAGE TupleSections, DeriveGeneric #-}
module Gen.IO.Dups(solveDups,deleteDups, Dup(..)) where

import Gen.Imports
import Data.Digest.Pure.MD5
import System.FilePath(replaceExtension)
import Data.Map(Map)

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M


data DirData = DirData {
      spec    :: MD5Digest
    , eprimes :: [(MD5Digest,MD5Digest)] -- eprime, error
    } deriving (Show,Eq,Generic,Ord)

data Dup = SolveDup{
      dupPath :: Directory
    , orgPath :: Directory
    } deriving (Show, Eq, Ord)

instance Pretty Dup where
    pretty = pretty . show

deleteDups :: MonadIO m => [Dup] -> m ()
deleteDups = liftIO . mapM_ ( removeDirectoryRecursive . dupPath)

-- | Given a set of dirs return the duplicates
solveDups :: (MonadIO m, Functor m) => [Directory] -> m [Dup]
solveDups dirs = do
  hashes <- mapM (\d -> ( (d,) <$>) <$> makeData d) dirs >>= return . catMaybes
  process (M.empty) hashes


process :: (MonadIO m, Functor m) => Map DirData Directory -> [(Directory,DirData)] -> m [Dup]
process _ []              = return []
process done ((dir,x):xs) = f done where
  f ms | Just org <- x `M.lookup` ms = do
    let dup = SolveDup{dupPath=dir,orgPath=org}
    (:) dup <$> process ms xs

  f ms = do
    let ms1 = M.insert x dir ms
    process ms1 xs


makeData :: (MonadIO m) => Directory -> m (Maybe DirData)
makeData dir = do
  con (dir </> "spec.essence") $ \spec_hash -> do
    eps <- liftIO $ allFilesWithSuffix ".eprime" dir
    h_eps <- mapM hashIfExists eps >>= return . catMaybes
    h_os  <- mapM (hashIfExists . (flip replaceExtension) ".output") eps
             >>= return . catMaybes
    if length eps /= length h_eps || length eps /= length h_os  then
        return Nothing
    else
        return . Just $ DirData{spec=spec_hash, eprimes=zip h_eps h_os}


  where
    con fp func = hashIfExists fp >>= \case
                  Nothing   -> return Nothing
                  Just hasher  -> func hasher


hashIfExists :: MonadIO m => FilePath -> m (Maybe MD5Digest)
hashIfExists fp =
  liftIO $ doesFileExist fp >>= \case
    False -> return Nothing
    True  -> Just <$> hashFileStrict fp

hashFileStrict :: FilePath -> IO MD5Digest
hashFileStrict fp = do
 content <- C.readFile fp
 let slns = C.concat $ [ x | x <- C.lines content
                        , not $ "###" `C.isPrefixOf` x
                        , not $ "+"   `C.isPrefixOf` x
                        , not $ "Created information file" `C.isPrefixOf` x
                        , not $ "Created output file" `C.isPrefixOf` x
                        , not $ "Created solution file" `C.isPrefixOf` x
                        ]
 return $ hash' slns
