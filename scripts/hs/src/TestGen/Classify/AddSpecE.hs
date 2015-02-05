{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TestGen.Classify.AddSpecE where

import TestGen.Classify.Sorter(getRecursiveContents)
import TestGen.Prelude

import Language.E.Pipeline.ReadIn(readSpecFromFile)
import System.FilePath (replaceExtension, takeExtension)


specEMain :: [FilePath] -> IO ()
specEMain = \case
   []     ->  putStrLn "gen specE <dir+>"
   [x]    ->  addSpecE x
   (x:xs) ->  addSpecE x >> specEMain xs


addSpecE :: FilePath -> IO ()
addSpecE fp_ = do
  specs_ :: [FilePath] <- ffind fp_
  specs  :: [Spec]    <- mapM readSpecFromFile specs_

  void $ zipWithM f specs specs_

  where
  f spec fp = do
    let specE  = fromSpec spec

    case specE of
      Left r -> error . show . vcat $ ["Error for " <+> pretty spec
                                      , "msg"  <+> (pretty . groom $ r)  ]
      Right r -> writeFile (replaceExtension fp ".specE" ) (show r)


ffind :: FilePath -> IO [FilePath]
ffind path = do
  names <- getRecursiveContents path
  filterM p names

  where
    p fp = do
      return $ (takeExtension $ fp) == ".essence"
