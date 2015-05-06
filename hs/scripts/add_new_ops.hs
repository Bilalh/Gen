{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

import Control.Applicative ((<$>))
import Data.List           (isSuffixOf, sort)
import Prelude
import System.Directory    (createDirectoryIfMissing, doesFileExist,
                            getDirectoryContents)
import System.Environment  (getArgs)
import System.FilePath     ((<.>), (</>))
import Text.Printf

import qualified Data.Set as S

usage :: String
usage = "runhaskell scripts/add_new_ops.hs $CONJURE_LIB/src/Conjure/Language/Expression/Op/"

skip :: S.Set String
skip = S.fromList [ "AttributeAsConstraint"
                  , "DontCare"
                  , "DotLeq"
                  , "DotLt"
                  , "Restrict"
                  , "TildeLeq"
                  , "TildeLt"
                  , "True"
                  , "Active"
                  , "generated"]

main :: IO ()
main = do
    opDir <- getArgs >>= \case
               [x] -> return x
               _ -> error $ "Usage:\n" ++ usage

    let outDir  = "src/Gen/Essence/Op"
    createDirectoryIfMissing True outDir
    operators_ <- sort . map (head . splitOn '.')
                       . filter (".hs" `isSuffixOf`)
                      <$> getDirectoryContents opDir
    let operators = S.toList  $ (S.fromList operators_ ) `S.difference` skip

    let opName m = "Op" ++ m
    let outText m = unlines $ concat
            [ [ "{-# OPTIONS_GHC -fno-warn-orphans #-}"
              , "module Gen.Essence.Op." ++ m ++ " where"
              , ""
              , "import Conjure.Language.Expression.Op"
              , "import Gen.Essence.St"
              , "import Gen.Helpers.StandardImports"
              , ""
              , ""
              ]

            , [ "instance Generate a => Generate (" ++ opName m ++ " a) where"
              , "  give GNone = do"
              , "    -- pick one of the possible return types for this op"
              , "    ty <- $notDone"
              , "    give (GType ty)"
              , ""
              , "  give (GType ty) = $notDone"
              , ""
              , "  give t = giveUnmatched \"Generate " ++ opName m ++ "\" t"
              , ""
              , "  -- Returns True if this op can be used with the specified return type"
              , "  -- and the remaing depth. This Op is counted in the depth calculation"
              , "  possible _ ty = $notDone"
              ]
            ]

    mapM_ (\m -> writeOut m (outDir </> m <.> ".hs" ) (outText m) ) operators



writeOut :: String -> FilePath -> String -> IO ()
writeOut m outFile outText= do
  doesFileExist outFile >>= \case
    True  -> return ()
    False -> do
      printf "Adding %-15s %s\n" m outFile
      writeFile outFile outText


splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn ch (ch2:rest) | ch == ch2 = splitOn ch rest
splitOn ch rest =
  let (before, after) = span (/=ch) rest
  in  before : splitOn ch after
