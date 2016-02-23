module Gen.Reduce.FormatResults(formatResults, copyDirectory) where

import Gen.Imports
import Gen.IO.Formats   (copyDirectory)
import Gen.IO.RunResult
import Gen.Reduce.Data
import System.Directory (renameDirectory)
import System.FilePath  (takeFileName)
import Gen.Helpers.MonadNote

formatResults :: (MonadIO m, MonadNote m)
              => Bool -> Bool -> RState -> m (Maybe FilePath)
formatResults delete_steps delete_others RState{rconfig=RConfig{..},..} = do

  res <- case mostReduced_ of
    Just r | (specDir r) /= specDir_ -> do
      noteFormat "Renaming" $
                   [ pretty (specDir r)
                   , " --> "
                   , pretty finalDir
                   ]
      liftIO $ renameDirectory  (specDir r) finalDir
      return $ Just finalDir

    _ -> do
      note "No final directory: no reductions produced"
      liftIO $ createDirectoryIfMissing True finalDir
      return $ Nothing

  case otherErrors_ of
    []  -> return ()
    xs  -> liftIO $  mapM_ classify xs

  files <- liftIO $ getDirectoryContents outputDir_
  let toMove = flip filter files
                 (`notElem` [
                    "others", "final", "zsteps", ".", "..", ".DS_Store", "versions.csv"
                  , "zreduce.logs", "meta.json","_reduced.logs", "_paramGen"] )
  liftIO $ createDirectoryIfMissing True stepsDir

  forM_ toMove $ \d -> do
    noteFormat "Renaming" $
             [ pretty  (outputDir_ </> d)
             , " --> "
             , pretty  (stepsDir </> d)
             ]
    liftIO $ renameDirectory (outputDir_ </> d) (stepsDir </> d)

  liftIO $ when delete_steps  $ removeDirectoryRecursive stepsDir
  -- removeDirectoryRecursive crashes when the directory is not there
  -- hack it by creating the directory if it is missing
  liftIO $ createDirectoryIfMissing True othersDir
  liftIO $ when delete_others $ removeDirectoryRecursive othersDir

  return res

  where

    finalDir  = outputDir_ </> "final"
    othersDir = outputDir_ </> "others"
    stepsDir  = outputDir_ </> "zsteps"

    classify :: ErrData -> IO ()
    classify r = do
      let newDir = othersDir </> (show (kind r)) </> (show (status r))
      createDirectoryIfMissing True newDir
      renameDirectory (specDir r) (newDir </> (takeFileName (specDir r)) )
