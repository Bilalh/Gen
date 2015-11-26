{-# LANGUAGE TupleSections, DeriveGeneric #-}
module Gen.IO.Dups( solveDups,refineDups
                  , deleteDups, deleteDups2, Dup(..)) where

import Crypto.Hash
import Data.Map           (Map)
import Gen.Imports hiding (hash)
import System.FilePath    (replaceExtension)

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M


data DirData = SolveData {
      spec    :: Digest MD5
    , eprimes :: [(Digest MD5, Digest MD5)] -- eprime, error
    }
    | RefineData {
      spec    :: Digest MD5
    , refines :: [Digest MD5] -- refinements errors
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

deleteDups2 :: MonadIO m => [Dup] -> m ()
deleteDups2 fps = liftIO . forM_ fps $ \dup -> do
                    putStrLn $ "Deleting " ++ (dupPath dup)
                    removeDirectoryRecursive (dupPath dup)


-- | Given a set of dirs return the duplicates
refineDups :: (MonadIO m, Functor m) => [Directory] -> m [Dup]
refineDups dirs = do
  hashes <- mapM (\d -> ( (d,) <$>) <$> makeData d) dirs >>= return . catMaybes
  processMap RefineDup (M.empty) hashes

  where
  makeData :: (MonadIO m) => Directory -> m (Maybe DirData)
  makeData dir = do
    invaild <- liftIO $ getAllFilesWithSuffix "solve_eprime.json" dir
    when (not $ null invaild) $ docError ["This is not a refinement error:", pretty dir]

    con (dir </> "spec.essence") $ \spec_hash -> do
      os <- liftIO $ getAllFilesWithSuffix ".refine-output" dir
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
    invaild <- liftIO $ getAllFilesWithSuffix "solve_eprime.json" dir
    when (null invaild) $ docError ["This is not a solving error:", pretty dir]

    con (dir </> "spec.essence") $ \spec_hash -> do
      eps <- liftIO $ getAllFilesWithSuffix ".eprime" dir
      h_eps <- mapM hashIfExists eps >>= return . catMaybes
      h_os  <- mapM (hashIfExists . (flip replaceExtension) ".output") eps
               >>= return . catMaybes
      if length eps /= length h_eps || length eps /= length h_os  then
          return Nothing
      else
          return . Just $ SolveData{spec=spec_hash, eprimes=zip h_eps h_os}

con :: MonadIO m => FilePath -> (Digest MD5 -> m (Maybe a)) -> m (Maybe a)
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


hashIfExists :: MonadIO m => FilePath -> m (Maybe (Digest MD5))
hashIfExists fp =
  liftIO $ doesFileExist fp >>= \case
    False -> return Nothing
    True  -> Just <$> hashFileStrict fp

hashFileStrict :: FilePath -> IO (Digest MD5)
hashFileStrict fp = do
 content <- C.readFile fp
 let slns = C.concat $ [ x | x <- C.lines content
                        -- run infomation
                        , not $ "###" `C.isPrefixOf` x
                        -- bash debuging
                        , not $ "+"   `C.isPrefixOf` x
                        -- eprime choices
                        , not $ "$"   `C.isPrefixOf` x
                        -- Conjure/SR Info
                        , not $ "Created information file" `C.isPrefixOf` x
                        , not $ "Created output file" `C.isPrefixOf` x
                        , not $ "Created solution file" `C.isPrefixOf` x
                        , not $ "Running with a timelimit of" `C.isPrefixOf` x
                        -- Filepaths
                        , not $ "    /home/bh246/" `C.isPrefixOf` x
                        , not $ "    /home/bilal/" `C.isPrefixOf` x
                        , not $ "    /home/ozgur/" `C.isPrefixOf` x
                        , not $ "    /Users/bilalh/" `C.isPrefixOf` x
                        -- for old versions
                        , not $ "conjureNew " `C.isPrefixOf` x
                        , not $ "conjureNew: " `C.isPrefixOf` x
                        -- for very old versions
                        , not $ "time conjureNew " `C.isPrefixOf` x
                        , not $ "time savilerow " `C.isPrefixOf` x
                        -- for very^2 old versions
                        , not $ "real\t" `C.isPrefixOf` x
                        , not $ "user\t" `C.isPrefixOf` x
                        , not $ "sys\t" `C.isPrefixOf` x
                        , not $ "%CPU (" `C.isInfixOf` x
                        , not $ "% cpu" `C.isInfixOf` x
                        , not $ "maxresident)" `C.isInfixOf` x
                        , not $ "pagefaults " `C.isInfixOf` x
                        ]
 return $ hash slns
