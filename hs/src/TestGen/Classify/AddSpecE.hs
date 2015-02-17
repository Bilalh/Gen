{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TestGen.Classify.AddSpecE where

import Conjure.UI.IO(readModelFromFile)
import Conjure.Language.Definition
import TestGen.Classify.Sorter(getRecursiveContents)
import TestGen.Prelude

import System.FilePath (replaceExtension, takeExtension)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L

specEMain :: [FilePath] -> IO ()
specEMain = \case
   []     ->  putStrLn "gen specE <dir+>"
   [x]    ->  addSpecE x
   (x:xs) ->  addSpecE x >> specEMain xs

inlineParamAndLettings :: Model -> Maybe Model -> Model
inlineParamAndLettings spec param = $notDone

addSpecE :: FilePath -> IO ()
addSpecE fp_ = do
  specs_ :: [FilePath] <- ffind fp_
  specs  :: [Model]    <- mapM readModelFromFile specs_

  void $ zipWithM f specs specs_

  where
  f spec fp = do
    -- let inlined = inlineParamAndLettings spec
        -- specE  = fromModel inlined
    let specE  = fromModel spec

    putStrLn fp

    case specE of
      Left r -> error . show . vcat $ ["Error for " <+> (pretty fp)
                                      , "spec"  <+> pretty spec
                                      , "msg"   <+> (pretty r)
                                      , "msg"   <+> (pretty . groom $ r)
                                      , "groom" <+> (pretty . groom $ spec)
                                      , "--"  ]
      Right r -> do
         -- b <- (compareSpecs r inlined)
         -- if not  b then do
         --     putStrLn "--mismatched--"
         --     putStrLn fp
         --     putStrLn . show . pretty $ inlined
         --     putStrLn . show . pretty $ r
         --     putStrLn . groom $ r

         --     putStrLn "--end--"
         -- else
         --     return ()

         writeFile (replaceExtension fp ".specE" )      (show r)
         L.writeFile (replaceExtension fp ".specE.json" ) (A.encode r)



compareSpecs :: Spec -> Model -> IO Bool
compareSpecs specE  m1  = do
    Model{mStatements=d} <- toModel specE
    let m2 = m1{mStatements=d}
    case hash m1 == hash m2 of
      True  -> return True
      False -> do
        return False


ffind :: FilePath -> IO [FilePath]
ffind path = do
  names <- getRecursiveContents path
  filterM p names

  where
    p fp = do
      return $ (takeExtension $ fp) == ".essence"
