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
              , "module Gen.Essence.Op." ++ m ++ " where"
              , ""
              , "import Conjure.Language.Expression.Op"
              , "import Gen.Essence.Rnd"
              , "import Gen.Essence.St"
              , "import Gen.Essence.Type               ()"
              , "import Gen.Helpers.SizeOf"
              , "import Gen.Imports"
              , ""
              , ""
              ]


            , [ "instance Generate a => Generate (" ++ opName m ++ " a) where"
              , "  give GNone = do"
              , "    -- pick one of the possible return types for this op"
              , "    ty <- $notDone"
              , "    give (GType ty)"
              , ""
              , "  -- When all return types are allowed "
              , "  -- give GNone = give GNone >>= \\ty -> give (GType ty)"
              , ""
              , "  give (GType ty) = $notDone"
              , ""
              , "  give t = giveUnmatched \"Generate " ++ opName m ++ "\" t"
              , ""
              , "  -- Returns True if this op can be used with the specified return type"
              , "  -- and the remaing depth. This Op is counted in the depth calculation."
              , "  -- Implement either possible or possiblePure:"
              , "  -- possiblePure is when the result only depends on the depth and the return type."
              , "  -- possible has access to the GenSt monad"
              , ""
              , "  -- possible _ ty = $notDone"
              , ""
              , "  -- possiblePure _ ty _ | ty /= TypeBool = False"
              , "  -- possiblePure _ ty d = depthOf ty + 1 <= (fromIntegral d)"
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
