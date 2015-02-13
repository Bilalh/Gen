{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TestGen.Classify.AddSpecE where

import Conjure.UI.IO(readModelFromFile)
import Conjure.Language.Definition(Model)
import TestGen.Classify.Sorter(getRecursiveContents)
import TestGen.Prelude

import System.FilePath (replaceExtension, takeExtension)

specEMain :: [FilePath] -> IO ()
specEMain = \case
   []     ->  putStrLn "gen specE <dir+>"
   [x]    ->  addSpecE x
   (x:xs) ->  addSpecE x >> specEMain xs


addSpecE :: FilePath -> IO ()
addSpecE fp_ = do
  specs_ :: [FilePath] <- ffind fp_
  specs  :: [Model]    <- mapM readModelFromFile specs_

  void $ zipWithM f specs specs_

  where
  f spec fp = do
    let inlined = fst $ inlineParamAndLettings spec Nothing
        specE  = fromSpec inlined

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

         writeFile (replaceExtension fp ".specE" ) (show r)


compareSpecs :: SpecE -> Model -> IO Bool
compareSpecs = error "compareSpecs"
-- compareSpecs specE  (Spec _ v2) = do
--     let (Spec lang v1) = toSpec specE
--         s1 = (Spec lang v1)
--         s2 = (Spec lang v2)
--     case hash s1 == hash s2 of
--       True  -> return True
--       False -> do
--         return False


ffind :: FilePath -> IO [FilePath]
ffind path = do
  names <- getRecursiveContents path
  filterM p names

  where
    p fp = do
      return $ (takeExtension $ fp) == ".essence"
