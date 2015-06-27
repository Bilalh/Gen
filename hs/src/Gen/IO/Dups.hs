{-# LANGUAGE TupleSections, DeriveGeneric #-}
module Gen.IO.Dups( solveDups,refineDups
                  , deleteDups, Dup(..)) where

import Gen.Imports
import Data.Digest.Pure.MD5
import System.FilePath(replaceExtension)
import Data.Map(Map)

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M


data DirData = SolveData {
      spec    :: MD5Digest
    , eprimes :: [(MD5Digest,MD5Digest)] -- eprime, error
    }
    | RefineData {
      spec    :: MD5Digest
    , refines :: [MD5Digest] -- refinements errors
    } deriving (Show,Eq,Generic,Ord)

data Dup = SolveDup{
      dupPath :: Directory
    , orgPath :: Directory
    }
    | RefineDup{
      dupPath :: Directory
    , orgPath :: Directory
    } deriving (Show, Eq, Ord)

instance Pretty Dup where
    pretty x = hang "Dup" 2 $ (vcat . map pretty) [ dupPath x, orgPath x]

deleteDups :: MonadIO m => [Dup] -> m ()
deleteDups = liftIO . mapM_ ( removeDirectoryRecursive . dupPath)


-- | Given a set of dirs return the duplicates
refineDups :: (MonadIO m, Functor m) => [Directory] -> m [Dup]
refineDups dirs = do
  hashes <- mapM (\d -> ( (d,) <$>) <$> makeData d) dirs >>= return . catMaybes
  processMap RefineDup (M.empty) hashes

  where
  makeData :: (MonadIO m) => Directory -> m (Maybe DirData)
  makeData dir = do
    con (dir </> "spec.essence") $ \spec_hash -> do
      os <- liftIO $ allFilesWithSuffix ".refine-output" dir
      h_os <- mapM hashIfExists os >>= return . catMaybes
      if length os /= length h_os  then
          return Nothing
      else
          return . Just $ RefineData{spec=spec_hash, refines= h_os}

-- | Given a set of dirs return the duplicates
solveDups :: (MonadIO m, Functor m) => [Directory] -> m [Dup]
solveDups dirs = do
  hashes <- mapM (\d -> ( (d,) <$>) <$> makeData d) dirs >>= return . catMaybes
  processMap SolveDup (M.empty) hashes

  where
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
          return . Just $ SolveData{spec=spec_hash, eprimes=zip h_eps h_os}

con :: MonadIO m => FilePath -> (MD5Digest -> m (Maybe a)) -> m (Maybe a)
con fp func = hashIfExists fp >>= \case
              Nothing   -> return Nothing
              Just hasher  -> func hasher


processMap :: (MonadIO m, Functor m)
           => (Directory -> Directory -> Dup)
           -> Map DirData Directory
           -> [(Directory,DirData)] -> m [Dup]
processMap _ _ []              = return []
processMap d done ((dir,x):xs) = f done where
  f ms | Just org <- x `M.lookup` ms = do
    let dup = d dir org
    (:) dup <$> processMap d ms xs

  f ms = do
    let ms1 = M.insert x dir ms
    processMap d ms1 xs


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
                        , not $ "$"   `C.isPrefixOf` x
                        , not $ "Created information file" `C.isPrefixOf` x
                        , not $ "Created output file" `C.isPrefixOf` x
                        , not $ "Created solution file" `C.isPrefixOf` x
                        , not $ "    /home/bh246/" `C.isPrefixOf` x
                        , not $ "    /home/bilal/" `C.isPrefixOf` x
                        , not $ "    /home/ozgur/" `C.isPrefixOf` x
                        , not $ "    /Users/bilalh/" `C.isPrefixOf` x
                        ]
 return $ hash' slns
