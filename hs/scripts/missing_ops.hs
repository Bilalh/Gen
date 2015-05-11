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
usage = "runhaskell scripts/missing_ops.hs $CONJURE_LIB/src/Conjure/Language/Expression/Op/"

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
    opDir <- getArgs >>= \case
               [x] -> return x
               _   -> error $ "Usage:\n" ++ usage


    let outDir  = "src/Gen/Essence/Op"
    createDirectoryIfMissing True outDir
    operators_ <- sort . map (head . splitOn '.')
                       . filter (".hs" `isSuffixOf`)
                      <$> getDirectoryContents opDir
    let operators = S.toList  $ (S.fromList operators_ ) `S.difference` skip

    mapM_ (\m -> printMissing m (outDir </> m <.> ".hs" ) ) operators


printMissing :: String -> FilePath  -> IO ()
printMissing m outFile = do
  doesFileExist outFile >>= \case
    True  -> return ()
    False -> do
      printf "Missing %-15s\n" m


splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn ch (ch2:rest) | ch == ch2 = splitOn ch rest
splitOn ch rest =
  let (before, after) = span (/=ch) rest
  in  before : splitOn ch after
