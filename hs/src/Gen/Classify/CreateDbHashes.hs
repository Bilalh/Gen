module Gen.Classify.CreateDbHashes(createDbHashesMain) where

import Gen.Classify.Sorter (getRecursiveContents)
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.RunResult
import System.FilePath     (takeExtensions, takeDirectory)

import qualified Control.Exception   as Exc
import qualified Data.IntSet as I
import qualified Data.HashMap.Strict as H


createDbHashesMain :: FilePath -> FilePath -> IO ()
createDbHashesMain dir out = do
  fps <- ffind dir
  hashesMay <- (flip concatMapM) fps $ (\fp -> do
    a <- catch1 fp $ readSpecHash  fp
    b <- catch2 fp $ readModelHash fp
    return [a,b]
    )

  let hashes = catMaybes hashesMay
  let skipping = I.fromList $ hashes

  let dbDef :: ResultsDB = def

  fdbs <- findDBs out
  groomPrint $ fdbs
  olds <- forM fdbs $ \cur -> do
    readFromJSON cur >>= \case
      Nothing -> return []
      Just (ResultsDB{resultsSkipped=rs
             ,resultsPassing=Mapped rp
             ,resultsErrors=Mapped re}) -> do
        return $[ I.fromList . H.keys $ rp
                , I.fromList . map fst3 . H.keys $ re
                , rs ]

  groomPrint $ olds

  let db = dbDef{resultsSkipped= I.unions $ skipping : concat olds }


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
  filterM p $ names

  where
    p fp = do
      return $ (takeExtensions $ fp)  == ".spec.json"
