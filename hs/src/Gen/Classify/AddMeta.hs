{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Gen.Classify.AddMeta where

import Gen.Classify.Meta(mkMeta)
import Gen.Classify.Sorter(getRecursiveContents)
import Gen.IO.Formats
import Gen.Prelude

import System.FilePath (takeExtension)

metaMain :: [FilePath] -> IO ()
metaMain = \case
   []     ->  putStrLn "gen meta <dir+>"
   [x]    ->  addMeta x
   (x:xs) ->  addMeta x >> metaMain xs


addMeta :: FilePath -> IO ()
addMeta fp_ = do
  specs_ :: [FilePath] <- ffind fp_
  specs  :: [Spec]    <- mapM readFromJSON specs_

  void $ zipWithM f specs specs_


  where
  f spec fp = do
    putStrLn fp
    let meta = mkMeta spec
    writeFile (replaceExtensions fp ".meta" ) (show meta)
    writeToJSON (replaceExtensions fp ".meta.json" ) (meta)


ffind :: FilePath -> IO [FilePath]
ffind path = do
  names <- getRecursiveContents path
  filterM p names

  where
    p fp = do
      return $ (takeExtension $ fp)  == ".spec.json"