module Gen.Classify.CreateDbHashes(createDbHashesMain) where

import Gen.Classify.Sorter (getRecursiveContents)
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.RunResult
import System.FilePath     (takeExtensions, takeDirectory)

import qualified Control.Exception   as Exc
import qualified Data.IntSet as I


createDbHashesMain :: FilePath -> FilePath -> IO ()
createDbHashesMain dir out = do
  fps <- ffind dir
  hashesMay <- (flip concatMapM) fps $ (\fp -> do
    a <- catch1 fp $ readSpecHash  fp
    b <- catch2 fp $ readModelHash fp
    return [a,b]
    )

  let hashes = nub2 $ catMaybes hashesMay
  let passing = I.fromList $ hashes

  let dbDef :: ResultsDB = def
  let db = dbDef{resultsSkipped= passing}

  writeDB_ False (Just out) db


  where
  catch1, catch2 :: FilePath -> IO (Maybe Hash) -> IO (Maybe Hash)
  catch1 fp f = Exc.catch f (handler "  FAILED(.spec.json): " fp)
  catch2 fp f = Exc.catch f (handler "  FAILED(spec.essence): " fp)

  handler :: String -> FilePath -> Exc.SomeException -> IO (Maybe Hash)
  handler prefix f _ = do
    putStrLn $ prefix ++ f
    return Nothing


readSpecHash :: FilePath -> IO (Maybe Hash)
readSpecHash fp = do
  sp :: Spec <- readFromJSON fp
  return $ Just $ hash sp

readModelHash :: FilePath -> IO (Maybe Hash)
readModelHash fp = do
  let model = takeDirectory fp </> "spec.essence"
  doesFileExist model >>= \case
    False -> return Nothing
    True  -> do
      sp <- readSpecFromEssence model
      return $ Just $ hash sp



ffind :: FilePath -> IO [FilePath]
ffind path = do
  names <- getRecursiveContents path
  filterM p names

  where
    p fp = do
      return $ (takeExtensions $ fp)  == ".spec.json"
