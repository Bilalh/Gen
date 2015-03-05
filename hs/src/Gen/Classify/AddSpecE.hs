{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Gen.Classify.AddSpecE where

import Conjure.Language.TH
import Conjure.Language.NameResolution(resolveNames)
import Conjure.UI.IO(readModelFromFile)
import Conjure.Language.Definition

import Gen.Classify.Sorter(getRecursiveContents)
import Gen.Prelude
import Gen.IO.Formats


import System.FilePath ( takeExtension)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L

-- import Gen.AST.TH
-- ee3 [essencee| false /\ &e |] = e
-- dd3 [domainn| set of &f |] = f

specEMain :: Bool ->  [FilePath] -> IO ()
specEMain printSpecs = \case
   []     ->  putStrLn "gen specE <dir+>"
   [x]    ->  addSpecE printSpecs x
   (x:xs) ->  addSpecE printSpecs x >> specEMain printSpecs xs


addSpecE :: Bool -> FilePath -> IO ()
addSpecE printSpecs fp_ = do
  specs_ :: [FilePath] <- ffind fp_
  specs  :: [Model]    <- mapM readModelFromFile specs_

  void $ zipWithM f specs specs_

  where
  f spec fp = do
    start <- ignoreLogs $ resolveNames spec >>= return . removeTrueConstraints
    let inlined = inlineParamAndLettings start Nothing
    let specE  = fromModel inlined

    putStrLn fp

    case specE of
      Left r -> error . show . vcat $ ["Error for " <+> (pretty fp)
                                      , "spec"  <+> pretty spec
                                      , "msg"   <+> (pretty r)
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

         writeFile (replaceExtensions fp ".spec" )      (show r)
         L.writeFile (replaceExtensions fp ".spec.json" ) (A.encode r)



removeTrueConstraints :: Model -> Model
removeTrueConstraints m =
   let flitered = map f (mStatements m)
   in m{mStatements=flitered}

   where
     f (SuchThat es) = SuchThat $ filter g es
     f s =s

     g [essence| true(&_) |] = False
     g _ = True

-- FIXME param file
inlineParamAndLettings :: Model -> Maybe Model -> Model
inlineParamAndLettings spec Nothing = inlineLettings spec
inlineParamAndLettings _ (Just _) = $notDone


inlineLettings :: Model -> Model
inlineLettings model =
    let
        inline p@(Reference nm _) = do
            x <- gets (lookup nm)
            return (fromMaybe p x)
        inline p = return p

        statements = catMaybes
                        $ flip evalState []
                        $ forM (mStatements model)
                        $ \ st ->
            case st of
                Declaration (Letting nm x)
                    -> modify ((nm,x) :) >> return Nothing
                -- The following doesn't work when the identifier is used in a domain
                -- Declaration (Letting nm x@Reference{})
                --     -> modify ((nm,x) :) >> return Nothing
                _ -> Just <$> transformBiM inline st
    in
        model { mStatements = statements }


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
