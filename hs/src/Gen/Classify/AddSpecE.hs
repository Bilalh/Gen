{-# LANGUAGE DeriveDataTypeable, QuasiQuotes #-}
module Gen.Classify.AddSpecE(specEMain, addSpecJson, compareSpecs) where

import Conjure.Language.Definition
import Conjure.Language.Expression.Op  (Op (MkOpTrue))
import Conjure.Language.NameResolution (resolveNames)
import Conjure.UI.IO                   (readModelFromFile)
import Conjure.UI.TypeCheck            (typeCheckModel)
import Gen.Classify.Sorter             (getRecursiveContents)
import Gen.IO.Formats
import Gen.Imports
import System.FilePath                 (takeExtension)
import Gen.Helpers.InlineLettings

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as L

specEMain :: Bool ->  [FilePath] -> IO ()
specEMain printSpecs = \case
   []     ->  putStrLn "gen json {-d <dir>}+"
   [x]    ->  addSpecJsons printSpecs x
   (x:xs) ->  addSpecJsons printSpecs x >> specEMain printSpecs xs


addSpecJsons :: Bool -> FilePath -> IO ()
addSpecJsons printSpecs = mapM_ (addSpecJson printSpecs)  <=< ffind

addSpecJson :: Bool -> FilePath -> IO ()
addSpecJson printSpecs fp = do
  spec <- readModelFromFile fp
  start <- ignoreLogs . runNameGen $ resolveNames spec >>= return . removeTrueConstraints

  case (ignoreLogs . runNameGen . typeCheckModel) start of
    Left x ->  error . show . vcat $
                [ "model failed type checking"
                , pretty fp
                , pretty x
                , pretty start
                , pretty . groom $ start
                ]

    Right{} -> do
      let inlined = inlineParamAndLettings start Nothing
      let specE  = fromModel inlined

      putStrLn ("    processing: " ++ fp)

      case specE of
        Left r -> error . show . vcat $ ["Error for " <+> (pretty fp)
                                        , "spec"  <+> pretty spec
                                        , "msg"   <+> (pretty r)
                                        , "groom" <+> (pretty . groom $ spec)
                                        , "--"  ]
        Right r -> do
           if printSpecs then
               putStrLn . show . vcat $ [
                              "Original"
                            , pretty spec
                            , "Converted"
                            , pretty r
                            ,  "Original AST"
                            , pretty . groom $ spec
                            , "Converted AST"
                            , pretty . groom $ inlined
                            , pretty . groom $ r
                            ]
           else
               return ()
           L.writeFile (replaceExtensions fp ".spec.json" ) (A.encode r)


removeTrueConstraints :: Model -> Model
removeTrueConstraints m =
   let flitered = map f (mStatements m)
   in m{mStatements=flitered}

   where
     f (SuchThat es) = SuchThat $ filter g es
     f s =s

     g (Op (MkOpTrue _)) = False
     g _ = True


inlineParamAndLettings :: Model -> Maybe Model -> Model
inlineParamAndLettings spec Nothing = inlineLettings spec
inlineParamAndLettings _ (Just _) = $notDone


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
