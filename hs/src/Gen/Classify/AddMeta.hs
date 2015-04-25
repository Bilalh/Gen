{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}

module Gen.Classify.AddMeta(metaMain, addMeta) where

import Gen.Classify.Meta(mkMeta)
import Gen.Classify.Sorter(getRecursiveContents)
import Gen.IO.Formats
import Gen.Prelude
import System.FilePath (takeExtensions)

metaMain :: [FilePath] -> IO ()
metaMain = \case
   []     ->  putStrLn "gen meta {-d <dir>}+"
   [x]    ->  addMetas x
   (x:xs) ->  addMetas x >> metaMain xs


addMetas :: FilePath -> IO ()
addMetas = mapM_ addMeta  <=< ffind

addMeta :: FilePath -> IO ()
addMeta fp = do
  spec :: Spec <- readFromJSON fp
  putStrLn ("    processing: " ++ fp)
  let meta = mkMeta spec
  writeToJSON (replaceExtensions fp ".meta.json" ) (meta)


ffind :: FilePath -> IO [FilePath]
ffind path = do
  names <- getRecursiveContents path
  filterM p names

  where
    p fp = do
      return $ (takeExtensions $ fp)  == ".spec.json"
