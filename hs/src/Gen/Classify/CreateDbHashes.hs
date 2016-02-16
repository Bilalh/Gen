module Gen.Classify.CreateDbHashes(createDbHashesMain) where

import Conjure.UI.IO       (readModelFromFile)
import Gen.Classify.Sorter (getRecursiveContents)
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.RunResult
import System.FilePath     (takeExtensions)

import qualified Control.Exception as Exc
import qualified Data.IntSet       as I


createDbHashesMain :: FilePath -> FilePath -> IO ()
createDbHashesMain dir out = do
  fps <- ffind dir
  hashesMay <- (flip mapM) fps $ (\fp ->
      catch2 fp $ readModelHash fp
    )

  let hashes = catMaybes hashesMay
  let skipping = I.fromList $ hashes

  let newDb :: ResultsDB = def{resultsSkipped=skipping, resultsSpecs=skipping}
  fdbs <- findDBs out
  currentDbs :: [ResultsDB] <-  catMaybes <$> mapM (readFromJSON) fdbs
  let merged = mconcat $ newDb : currentDbs
  writeDB_ False (Just out) merged

  where
  catch2 :: FilePath -> IO (Maybe SpecHash) -> IO (Maybe SpecHash)
  catch2 fp f = Exc.catch f (handler "  FAILED(spec.essence): " fp)

  handler :: String -> FilePath -> Exc.SomeException -> IO (Maybe SpecHash)
  handler prefix f _ = do
    putStrLn $ prefix ++ f
    return Nothing


readModelHash :: FilePath -> IO (Maybe SpecHash)
readModelHash fp = do
  doesFileExist fp >>= \case
    False -> return Nothing
    True  -> do
      model <- readModelFromFile fp
      return $ Just $ hashDoc model


findDBs :: FilePath -> IO [FilePath]
findDBs path = do
  names <- getDirectoryContents path
  filterM p $ map (path </>) names

  where
    p fp = do
      return $ (takeExtensions $ fp)  == ".json"


ffind :: FilePath -> IO [FilePath]
ffind path = do
  names <- getRecursiveContents path
  return $ filter p $ names

  where
    p fp =  (takeExtensions $ fp)  == ".essence"
         || (takeExtensions $ fp)  == ".param"
