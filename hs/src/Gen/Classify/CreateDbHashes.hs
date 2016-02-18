module Gen.Classify.CreateDbHashes(createDbHashesMain) where

import Conjure.UI.IO    (readModelFromFile)
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.RunResult
import System.FilePath  (takeExtensions)

import qualified Control.Exception   as Exc
import qualified Data.HashMap.Strict as H
import qualified Data.IntSet         as I


createDbHashesMain :: Bool -> Bool -> Bool -> Bool
                   -> FilePath -> FilePath
                   -> IO ()
createDbHashesMain
  delete_passing delete_errors delete_skipped errors_to_skipped
    dir out = do
  specHashes  <- filesWithExtension ".essence" dir >>= hashesMay
  paramHashes <- filesWithExtension ".param" dir   >>= hashesMay
  let curSpecs   = I.fromList (catMaybes specHashes)
  let curSkipped = I.fromList (catMaybes paramHashes) `I.union` curSpecs

  fdbs <- findDBs out
  let newDb  :: ResultsDB    = def{resultsSkipped=curSkipped, resultsSpecs=curSpecs}
  currentDbs :: [ResultsDB] <- catMaybes <$> mapM (readFromJSON) fdbs
  let ResultsDB{..} = mconcat $ newDb : currentDbs

  let resultsSkipped1 =
        if   errors_to_skipped
        then resultsSkipped `I.union`  (I.fromList $ errorHashes resultsErrors)
        else resultsSkipped


  let rsp = resultsSpecs
            I.\\ (iff delete_skipped resultsSkipped1 )
            I.\\ (iff delete_passing (I.fromList $ passingSpecHashes resultsPassing))
            I.\\ (iff (delete_errors && not errors_to_skipped)
                   (I.fromList $ errorSpecHashes resultsErrors) )

  -- TODO actually remove the dirs of the errors
  let res = ResultsDB{ resultsSpecs   = rsp
                     , resultsSkipped = neg delete_skipped resultsSkipped1
                     , resultsErrors  = neg (delete_errors || errors_to_skipped)
                                             resultsErrors
                     , resultsPassing = neg delete_passing resultsPassing}

  when (delete_errors || errors_to_skipped ) $ do
    let f (Mapped x) = x
    let fps = [ specDir | (StoredError ErrData{specDir}) <- H.elems (f resultsErrors) ]
    forM_ fps $ \fp -> do
      doesDirectoryExist (out </> fp)  >>= \case
        False -> return ()
        True -> removeDirectoryRecursive (out </> fp)

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
