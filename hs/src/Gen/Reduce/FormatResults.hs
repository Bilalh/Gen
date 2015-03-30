module Gen.Reduce.FormatResults(formatResults, copyDirectory) where

import Gen.Prelude
import Gen.Reduce.Data
import System.Directory (copyFile, renameDirectory)
import System.FilePath  (takeFileName)

formatResults :: Bool -> RState -> IO ()
formatResults delete_steps RState{..} = do

  case mostReduced_ of
    Just r -> do
      renameDirectory  (resDirectory_ r) finalDir

    Nothing -> do
      return ()

  writeFile (finalDir </> "zreduce.logs") (renderSized 120 rlogs_)

  case otherErrors_ of
    []  -> return ()
    xs  -> mapM_ classify xs

  files <- getDirectoryContents outputDir_
  let toMove = flip filter files
                 (`notElem` [
                   "others", "final", "zsteps", ".", ".."
                  , "zreduce.logs", "versions.csv", "meta.json","_reduced.logs"] )
  createDirectoryIfMissing True stepsDir

  forM_ toMove $ \d -> do
    renameDirectory (outputDir_ </> d) (stepsDir </> d)

  if delete_steps then
      removeDirectoryRecursive stepsDir
  else
      return ()

  where

    finalDir  = outputDir_ </> "final"
    othersDir = outputDir_ </> "others"
    stepsDir  = outputDir_ </> "zsteps"

    classify :: RunResult -> IO ()
    classify r = do
      let newDir = othersDir </> (show (resErrKind_ r)) </> (show (resErrStatus_ r))
      createDirectoryIfMissing True newDir
      renameDirectory (resDirectory_ r) (newDir </> (takeFileName (resDirectory_ r)) )


copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory from to = do
  createDirectoryIfMissing True to
  fps <- (getDirectoryContents from)
  forM_ (filter (`notElem` [".", ".."])  fps) $ \f -> do
    doesDirectoryExist f >>= \case
      True  -> return ()
      False -> copyFile (from </> f) (to </> f)
