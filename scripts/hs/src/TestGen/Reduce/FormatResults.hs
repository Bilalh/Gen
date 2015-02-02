{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module TestGen.Reduce.FormatResults(formatResults) where

import TestGen.Reduce.Data
import TestGen.Prelude(renderSized)

import System.FilePath((</>), takeFileName)
import System.Directory(createDirectoryIfMissing, renameDirectory
                       ,getDirectoryContents,removeDirectoryRecursive
                       , doesDirectoryExist, copyFile)
import Control.Monad(forM_)

formatResults :: RState -> IO ()
formatResults RState{..} = do
  writeFile (outputDir_ </> "run.logs") (renderSized 120 rlogs_)

  case mostReduced_ of
    Just RunResult{..} -> do
      renameDirectory  resDirectory_ finalDir

    Nothing -> do
      if True then
          return ()
       else
           copyDirectory specDir_ finalDir

  case otherErrors_ of
    []  -> return ()
    xs  -> mapM_ classify xs

  files <- getDirectoryContents outputDir_
  let toDelete = flip filter files
                 (`notElem` ["others", "final", "zsteps", ".", "..", "run.logs"] )
  createDirectoryIfMissing True stepsDir
  forM_ toDelete $ \d -> do
    -- removeDirectoryRecursive (outputDir_ </> d)
    renameDirectory (outputDir_ </> d) (stepsDir </> d)


  where

    finalDir  = outputDir_ </> "final"
    othersDir = outputDir_ </> "others"
    stepsDir  = outputDir_ </> "zsteps"

    classify :: RunResult -> IO ()
    classify (RunResult{..}) = do
      let newDir = othersDir </> (show resErrKind_) </> (show resErrStatus_)
      createDirectoryIfMissing True newDir
      renameDirectory resDirectory_ (newDir </> (takeFileName resDirectory_) )

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory from to = do
  createDirectoryIfMissing True to
  fps <- (getDirectoryContents from)
  forM_ (filter (`notElem` [".", ".."])  fps) $ \f -> do
    doesDirectoryExist f >>= \case
      True  -> return ()
      False -> copyFile (from </> f) (to </> f)
