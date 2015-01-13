{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module TestGen.Reduce.FormatResults where

import TestGen.Reduce.Data
-- import TestGen.Prelude

import System.FilePath((</>), takeFileName)
import System.Directory(createDirectoryIfMissing, renameDirectory
                       ,getDirectoryContents,removeDirectoryRecursive)
import Control.Monad(forM_)

formatResults :: RState -> IO ()
formatResults RState{..} = do
  case mostReduced_ of
    Just RunResult{..} -> do
      renameDirectory  resDirectory_ finalDir

    Nothing -> return ()

  case otherErrors_ of
    []  -> return ()
    xs  -> mapM_ classify xs

  files <- getDirectoryContents outputDir_
  let toDelete = filter (\f -> f `notElem` ["others", "final", ".", ".." ] ) files
  forM_ toDelete $ \d -> do
    removeDirectoryRecursive (outputDir_ </> d)


  where

    finalDir  = outputDir_ </> "final"
    othersDir = outputDir_ </> "others"

    classify :: RunResult -> IO ()
    classify (RunResult{..}) = do
      let newDir = othersDir </> (show resErrKind_) </> (show resErrStatus_)
      createDirectoryIfMissing True newDir
      renameDirectory resDirectory_ (newDir </> (takeFileName resDirectory_) )
