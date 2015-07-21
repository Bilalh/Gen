module Gen.Reduce.FormatResults(formatResults, copyDirectory) where

import Gen.Imports
import Gen.IO.Formats   (copyDirectory)
import Gen.IO.RunResult
import Gen.Reduce.Data
import System.Directory (renameDirectory)
import System.FilePath  (takeFileName)

formatResults :: Bool -> Bool -> RState -> IO (Maybe FilePath)
formatResults delete_steps delete_others RState{rconfig=RConfig{..},..} = do

  res <- case mostReduced_ of
    Just r -> do
      putStrLn . show . vcat $
                   [ "Renaming "
                   , pretty (specDir r)
                   , " --> "
                   , pretty finalDir
                   ]
      renameDirectory  (specDir r) finalDir
      return $ Just finalDir

    Nothing -> do
      putStrLn "No final directory: no reductions produced"
      createDirectoryIfMissing True finalDir
      return $ Nothing

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
    putStrLn . show . vcat $
             [ "Renaming "
             , pretty  (outputDir_ </> d)
             , " --> "
             , pretty  (stepsDir </> d)
             ]
    renameDirectory (outputDir_ </> d) (stepsDir </> d)

  when delete_steps  $ removeDirectoryRecursive stepsDir
  when delete_others $ removeDirectoryRecursive othersDir

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
