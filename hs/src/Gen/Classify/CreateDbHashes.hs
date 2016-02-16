module Gen.Classify.CreateDbHashes(createDbHashesMain) where

import Conjure.UI.IO    (readModelFromFile)
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.RunResult
import System.FilePath  (takeExtensions)

import qualified Control.Exception as Exc
import qualified Data.IntSet       as I


createDbHashesMain :: Bool -> Bool -> Bool
                   -> FilePath -> FilePath
                   -> IO ()
createDbHashesMain
  delete_passing delete_errors delete_skipped
  dir out = do
  specHashes  <- filesWithExtension ".essence" dir >>= hashesMay
  paramHashes <- filesWithExtension ".param" dir   >>= hashesMay
  let curSpecs   = I.fromList (catMaybes specHashes)
  let curSkipped = I.fromList (catMaybes paramHashes) `I.union` curSpecs

  fdbs <- findDBs out
  let newDb  :: ResultsDB    = def{resultsSkipped=curSkipped, resultsSpecs=curSpecs}
  currentDbs :: [ResultsDB] <- catMaybes <$> mapM (readFromJSON) fdbs
  let ResultsDB{..} = mconcat $ newDb : currentDbs

  let rsp = resultsSpecs
            I.\\ (iff delete_skipped resultsSkipped )
            I.\\ (iff delete_passing (I.fromList $ passingSpecHashes resultsPassing))
            I.\\ (iff delete_errors  (I.fromList $ errorSpecHashes resultsErrors) )

  -- TODO actually remove the dirs of the errors
  let res = ResultsDB{ resultsSpecs   = rsp
                     , resultsSkipped = neg delete_skipped resultsSkipped
                     , resultsErrors  = neg delete_errors  resultsErrors
                     , resultsPassing = neg delete_passing resultsPassing}

  -- putStrLn $ show . vcat $ [ nn "cur"   (groom currentDbs)
  --                          , nn "merged" (groom x)
  --                          , nn "res" (groom res)]

  writeDB_ False (Just out) res

  where
  iff bool val = if bool     then val else def
  neg bool val = if not bool then val else def


  catch2 :: FilePath -> IO (Maybe SpecHash) -> IO (Maybe SpecHash)
  catch2 fp f = Exc.catch f (handler "  FAILED: " fp)

  handler :: String -> FilePath -> Exc.SomeException -> IO (Maybe SpecHash)
  handler prefix f _ = do
    putStrLn $ prefix ++ f
    return Nothing

  hashesMay :: [FilePath] -> IO [Maybe SpecHash]
  hashesMay place = (flip mapM) place $ (\fp ->
      catch2 fp $ readModelHash fp
    )


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
