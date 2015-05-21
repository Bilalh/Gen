{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
-- Show the ops that have not been added yet
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
              , "  give GNone = do"
              , "    ty <- $notDone"
              , "    give (GType ty)"
              , ""
              , "  -- When all return types are allowed "
              , "  -- give GNone = give GNone >>= \\ty -> give (GType ty)"
              , ""
              , " -- e.g.  give ty@GType{} = pure OpUnion <*> give ty <*> give ty"
              , "  give (GType ty) = $notDone"
              , ""
              , "  give t = giveUnmatched \"Generate " ++ opName m ++ "\" t"
              , ""
              , "  -- The depth required for the children of the op, bool and int need 0"
              , "  -- possiblePure _ (Just ty) d = depthOf ty  <= (fromIntegral d)"
              , "  -- possiblePure _ _ d         = d >=1"
              , ""
              , "  -- requires _ (Just ty) = [RAll $ keyList ty] "
              , "  -- requires _ _         = [RAny $notDone]"
              , ""

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
