{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TestGen.Classify.AddMeta where

import TestGen.Classify.Meta(mkMeta)
import TestGen.Classify.Sorter(getRecursiveContents)
import TestGen.Helpers.Runner
import TestGen.Prelude
import TestGen.Reduce.Runner(readSpecE)

import System.FilePath (replaceExtension,takeExtension)

metaMain :: [FilePath] -> IO ()
metaMain = \case
   []     ->  putStrLn "gen meta <dir+>"
   [x]    ->  addMeta x
   (x:xs) ->  addMeta x >> metaMain xs


addMeta :: FilePath -> IO ()
addMeta fp_ = do
  specs_ :: [FilePath] <- ffind fp_
  specs  :: [SpecE]    <- mapM readSpecE specs_

  void $ zipWithM f specs specs_


  where
  f spec fp = do
    putStrLn fp
    let meta = mkMeta spec
    writeFile (replaceExtension fp ".meta" ) (show meta)
    writeJSON (replaceExtension fp ".meta.json" ) (meta)


ffind :: FilePath -> IO [FilePath]
ffind path = do
  names <- getRecursiveContents path
  filterM p names

  where
    p fp = do
      return $ (takeExtension $ fp)  == ".specE"
