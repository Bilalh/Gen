module Gen.Classify.CreateDbHashes(createDbHashesMain) where

import Conjure.UI.IO    (readModelFromFile)
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.RunResult
import System.FilePath  (takeExtensions)
import Data.Bifunctor(bimap)

import qualified Control.Exception   as Exc
import qualified Data.HashMap.Strict as H
import qualified Data.IntSet         as I


createDbHashesMain :: Bool -> Bool -> Bool -> Bool -> Bool
                   -> FilePath -> FilePath
                   -> IO ()
createDbHashesMain no_add
  delete_passing delete_errors delete_skipped errors_to_skipped
    dir out = do
  newDb <-
    if no_add then
      return def
    else do
      specHashes  <- filesWithExtension ".essence" dir >>= hashesMay
      paramHashes <- filesWithExtension ".param" dir   >>= hashesMay
      let curSpecs   = I.fromList (catMaybes specHashes)
      let curParams  = I.fromList (catMaybes paramHashes)
      return def{rSpecs=curSpecs, rSpecSkipped=curSpecs, rParamSkipped=curParams}

  fdbs <- findDBs out
  currentDbs :: [ResultsDB] <- catMaybes <$> mapM (readFromJSON) fdbs
  let ResultsDB{..} = mconcat $ newDb : currentDbs

  let (rSpecSkipped1, rParamSkipped1) = bimap
       (\x -> rSpecSkipped  `I.union` (iff errors_to_skipped (I.fromList x)) )
       (\x -> rParamSkipped `I.union` (iff errors_to_skipped (I.fromList x)) )
       $ unzip $ errorHashes rErrors

  let rsp = rSpecs
            I.\\ (iff delete_skipped rSpecSkipped1 )
            I.\\ (iff delete_skipped rParamSkipped1 )
            I.\\ (iff delete_passing (I.fromList $ passingSpecHashes rPassing))
            I.\\ (iff (delete_errors && not errors_to_skipped)
                   (I.fromList $ errorSpecHashes rErrors) )

  let res = ResultsDB{ rSpecs        = rsp
                     , rErrors       = neg (delete_errors || errors_to_skipped) rErrors
                     , rPassing      = neg delete_passing rPassing
                     , rSpecSkipped  = neg delete_skipped rSpecSkipped1
                     , rParamSkipped = neg delete_skipped rParamSkipped1

                     }

  when (delete_errors || errors_to_skipped ) $ do
    let f (Mapped x) = x
    let fps = [ specDir | (StoredError ErrData{specDir}) <- H.elems (f rErrors) ]
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
