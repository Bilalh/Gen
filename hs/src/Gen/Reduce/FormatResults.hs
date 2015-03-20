module Gen.Reduce.FormatResults(formatResults, copyDirectory) where

import Gen.Prelude
import Gen.Reduce.Data
import System.Directory (copyFile, renameDirectory)
import System.FilePath  (takeFileName)

formatResults :: RState -> IO ()
formatResults RState{..} = do

  case mostReduced_ of
    Just r -> do
      renameDirectory  (resDirectory_ r) finalDir

    Nothing -> do
      if True then
          return ()
       else
           copyDirectory specDir_ finalDir

  writeFile (finalDir </> "zreduce.logs") (renderSized 120 rlogs_)

  case otherErrors_ of
    []  -> return ()
    xs  -> mapM_ classify xs

  files <- getDirectoryContents outputDir_
  let toDelete = flip filter files
                 (`notElem` [
                   "others", "final", "zsteps", ".", ".."
                  , "zreduce.logs", "versions.csv", "meta.json"] )
  createDirectoryIfMissing True stepsDir

  forM_ toDelete $ \d -> do
    renameDirectory (outputDir_ </> d) (stepsDir </> d)


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
