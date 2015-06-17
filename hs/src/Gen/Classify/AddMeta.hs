{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}

module Gen.Classify.AddMeta(metaMain, addMeta) where

import Gen.Classify.Meta(mkMeta)
import Gen.Classify.Sorter(getRecursiveContents)
import Gen.IO.Formats
import Gen.Imports
import System.FilePath (takeExtensions)

import qualified Control.Exception as Exc


metaMain :: Bool ->  [FilePath] -> IO ()
metaMain verbose = \case
   []     ->  putStrLn "gen meta {-d <dir>}+"
   [x]    ->  addMetas verbose x
   (x:xs) ->  addMetas verbose x >> metaMain verbose xs

addMetas :: Bool -> FilePath -> IO ()
addMetas verbose = ffind >=>
  mapM_ (\fp -> do
           when verbose $  putStrLn ("    processing: " ++ fp)
           catch fp $ addMeta fp
        )

  where
  catch :: FilePath -> IO () -> IO ()
  catch fp f = Exc.catch f (handler fp)

  handler ::  FilePath -> Exc.SomeException -> IO ()
  handler f e = do
    putStrLn $ "  FAILED(.meta.json): " ++ f
    when verbose (print e)

addMeta :: FilePath -> IO ()
addMeta fp = do
  spec :: Spec <- readFromJSON fp
  meta <- mkMeta spec
  writeToJSON (replaceExtensions fp ".meta.json" ) (meta)


ffind :: FilePath -> IO [FilePath]
ffind path = do
  names <- getRecursiveContents path
  filterM p names

  where
    p fp = do
      return $ (takeExtensions $ fp)  == ".spec.json"
