{-# LANGUAGE DeriveDataTypeable, QuasiQuotes #-}
module Gen.Classify.AddSpecE(specEMain, addSpecJson, compareSpecs) where

import Conjure.Language.Definition
import Conjure.UI.IO                   (readModelFromFile)
import Conjure.UI.TypeCheck            (typeCheckModel)
import Gen.Classify.Sorter             (getRecursiveContents)
import Gen.Helpers.InlineLettings
import Gen.Imports
import Gen.IO.Formats
import System.FilePath                 (takeExtension)
import Conjure.UI.TypeCheck(typeCheckModel_StandAlone)
import Conjure.Language.NameResolution   (resolveNames)

import qualified Control.Exception    as Exc
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as L



specEMain :: Bool -> Bool ->  [FilePath] -> IO ()
specEMain verbose printSpecs = \case
   []     ->  putStrLn "gen json {-d <dir>}+"
   [x]    ->  addSpecJsons verbose printSpecs x
   (x:xs) ->  addSpecJsons verbose printSpecs x >> specEMain verbose printSpecs xs


addSpecJsons :: Bool -> Bool -> FilePath -> IO ()
addSpecJsons verbose printSpecs = ffind >=>
  mapM_ (\fp -> do
           when verbose $   putStrLn ("    processing: " ++ fp)
           catcher fp $ addSpecJson printSpecs fp
        )

  where
  catcher :: FilePath -> IO () -> IO ()
  catcher fp f = Exc.catch f (handler fp)

  handler :: FilePath -> Exc.SomeException -> IO ()
  handler f e= do
    putStrLn $ "  FAILED(.spec.json): " ++ f
    when verbose (print e)


addSpecJson :: Bool -> FilePath -> IO ()
addSpecJson printSpecs fp = do
  model <- readModelFromFile fp
  -- Not sure why I can't just do case (ignoreLogs . runNameGen . typeCheckModel_StandAlone)
  -- start <- ignoreLogs . runNameGen $ typeCheckModel_StandAlone model
  named <- ignoreLogs  . runNameGen  $  resolveNames model
  start <- ignoreLogs  . runNameGen  $  typeCheckModel named

  case (ignoreLogs . runNameGen . typeCheckModel_StandAlone) start of
    Left x ->  error . show . vcat $
                [ "model failed type checking"
                , pretty fp
                , pretty x
                , "start"
                , pretty start
                , "start"
                , pretty . groom $ start
                ]

    Right{} -> do
      when printSpecs $ putStrLn "Passed type checking"
      -- when printSpecs $ print . pretty $  start


      let inlined = inlineLettings start
      let specE  = fromModel inlined

      case specE of
        Left r -> error . show . vcat $ ["Error for " <+> (pretty fp)
                                        , "model"  <+> pretty model
                                        , "msg"   <+> (pretty r)
                                        , "groom" <+> (pretty . groom $ model)
                                        , "--"  ]
        Right r -> do
           when printSpecs $
               putStrLn . show . vcat $ [
                              "Original"
                            , pretty model
                            , "Converted"
                            , pretty r
                            ,  "Original AST"
                            , pretty . groom $ model
                            , "Converted AST"
                            , pretty . groom $ inlined
                            , pretty . groom $ r
                            ]
           L.writeFile (replaceExtensions fp ".spec.json" ) (A.encode r)

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
