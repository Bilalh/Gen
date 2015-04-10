{-# LANGUAGE DeriveDataTypeable, QuasiQuotes #-}

module Gen.Classify.AddSpecE where

import Conjure.Language.Definition
import Conjure.Language.Expression.Op  (Op (MkOpTrue))
import Conjure.Language.NameResolution (resolveNames)
import Conjure.UI.IO                   (readModelFromFile)
import Conjure.UI.TypeCheck            (typeCheckModel)
import Gen.Classify.Sorter             (getRecursiveContents)
import Gen.IO.Formats
import Gen.Prelude
import System.FilePath                 (takeExtension)

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as L

specEMain :: Bool ->  [FilePath] -> IO ()
specEMain printSpecs = \case
   []     ->  putStrLn "gen json {-d <dir>}+"
   [x]    ->  addSpecE printSpecs x
   (x:xs) ->  addSpecE printSpecs x >> specEMain printSpecs xs

addSpecE :: Bool -> FilePath -> IO ()
addSpecE printSpecs fp_ = do
  specs_ :: [FilePath] <- ffind fp_
  specs  :: [Model]    <- mapM readModelFromFile specs_

  void $ zipWithM f specs specs_

  where
  f spec fp = do
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

        statements :: [Statement]
        statements = catMaybes
                        $ flip evalState []
                        $ forM (mStatements model)
                        $ \(st :: Statement) ->
            case st of
                Declaration (Letting nm x)
                    -> modify ((nm,x) :) >> return Nothing
                -- The following doesn't work when the identifier is used in a domain
                -- Declaration (Letting nm x@Reference{})
                --     -> modify ((nm,x) :) >> return Nothing
                st' -> Just <$> transformBiM inline (st' :: Statement)
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
