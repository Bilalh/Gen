module Gen.Classify.CreateDbHashes(createDbHashesMain) where

import Gen.Classify.Sorter (getRecursiveContents)
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.RunResult
import System.FilePath     (takeExtensions)

import qualified Control.Exception   as Exc
import qualified Data.HashMap.Strict as H


createDbHashesMain :: FilePath -> FilePath -> IO ()
createDbHashesMain dir out = do
  fps <- ffind dir
  hashesMay <- forM fps $ (\fp -> do
    catch fp $ readSpecHash fp
    )
  let hashes = catMaybes hashesMay

  let passing = H.fromList $ zip hashes (repeat 0)

  let dbDef :: ResultsDB = def
  let db = dbDef{resultsPassing=Mapped $ passing}

  writeDB_ True (Just out) db


  where
  catch :: FilePath -> IO (Maybe Hash) -> IO (Maybe Hash)
  catch fp f = Exc.catch f (handler fp)

  handler :: FilePath -> Exc.SomeException -> IO (Maybe Hash)
  handler f _ = do
    putStrLn $ "  FAILED(.spec.json): " ++ f
    return Nothing


readSpecHash :: FilePath -> IO (Maybe Hash)
readSpecHash fp = do
  sp :: Spec <- readFromJSON fp
  return $ Just $ hash sp


ffind :: FilePath -> IO [FilePath]
ffind path = do
  names <- getRecursiveContents path
  filterM p names

  where
    p fp = do
      return $ (takeExtensions $ fp)  == ".spec.json"
