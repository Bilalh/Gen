{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
-- Show the ops that have not been added yet
import Control.Applicative ((<$>))
import Control.Monad       (when)
import Data.List           (isSuffixOf, sort)
import Prelude
import System.Directory    (createDirectoryIfMissing, doesFileExist,
                            getDirectoryContents)
import System.Environment  (getArgs)
import System.FilePath     ((<.>), (</>))
import Text.Printf
import System.Exit(exitFailure)

import qualified Data.Set as S

usage :: String
usage = "runhaskell scripts/add_new_ops.hs $CONJURE_LIB/src/Conjure/Language/Expression/Op/  [<Ops to use>+]"

skip :: S.Set String
skip = S.fromList [ "AttributeAsConstraint"
                  , "Active"
                  , "DontCare"
                  , "DotLeq"
                  , "DotLt"
                  , "generated"
                  , "Restrict"
                  , "TildeLeq"
                  , "TildeLt"
                  , "True"
                  ]

main :: IO ()
main = do
    (opDir, only) <- getArgs >>= \case
               []  -> error $ "Usage:\n" ++ usage
               [x] -> return (x,[])
               (x:xs) -> return (x,xs)


    let outDir  = "src/Gen/Essence/Op"
    createDirectoryIfMissing True outDir
    operators_ <- sort . map (head . splitOn '.')
                       . filter (".hs" `isSuffixOf`)
                      <$> getDirectoryContents opDir
    let operators =
          case only of
            [] -> S.toList  $ (S.fromList operators_ ) `S.difference` skip
            xs -> S.toList  $ (S.fromList operators_ ) `S.intersection` (S.fromList xs)

    when (null operators) $ do
      putStrLn $ "No matching ops found for args:" ++ show (only)
      exitFailure

    let opName m = "Op" ++ m
    let outText m = unlines $ concat
            [ [ "{-# OPTIONS_GHC -fno-warn-orphans #-}"
              , "module Gen.Essence.Op." ++ m ++ " (Generate(..)) where"
              , ""
              , "import Conjure.Language.AdHoc"
              , "import Conjure.Language.Expression.Op"
              , "import Gen.Essence.Id"
              , "import Gen.Essence.Rnd"
              , "import Gen.Essence.St"
              , "import Gen.Essence.Type               ()"
              , "import Gen.Helpers.SizeOf"
              , "import Gen.Imports"
              , ""
              , "import qualified Gen.Essence.Data.Types as Types"
              , ""
              ]


            , [ "instance (Generate a, ExpressionLike a) => Generate (" ++ opName m ++ " a) where"
              , "  give (GType ty) = do"
              , "    _1"
              , ""
              , "  give t = giveUnmatched \"Generate " ++ opName m ++ "\" t"
              , ""
              , "  -- Commonly fromIntegral d >= depthOf ty"
              , "  possiblePure _ (Just ty) d = _2"
              , "  possiblePure _ Just{} _    = False"
              , "  possiblePure _ Nothing _   = False"
              , ""
              , "  -- Commonly  [RAll $ keyList ty]"
              , "  requires _ (Just ty) = _3"
              , "  requires _ _         = []"
              , ""

              ]
            ]

    mapM_ (\m -> writeOut m (outDir </> m <.> ".hs" ) (outText m) ) operators



writeOut :: String -> FilePath -> String -> IO ()
writeOut m outFile outText= do
  doesFileExist outFile >>= \case
    True  -> do
      printf "Resuing %-15s %s\n" m outFile
    False -> do
      printf "Adding %-15s %s\n" m outFile
      writeFile outFile outText


splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn ch (ch2:rest) | ch == ch2 = splitOn ch rest
splitOn ch rest =
  let (before, after) = span (/=ch) rest
  in  before : splitOn ch after
