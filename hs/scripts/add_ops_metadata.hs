{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

import Control.Applicative ((<$>))
import Control.Exception   (IOException, catch)
import Data.List           (isSuffixOf, sort, intercalate,intersperse)
import Prelude
import System.Directory    (createDirectoryIfMissing, getDirectoryContents)
import Text.Printf


main :: IO ()
main = do
    let opDir    = "src/Gen/Essence/Op"
    let outDir   = "src/Gen/Essence/Op/Internal"
    let outFile  = "src/Gen/Essence/Op/Internal/Generated.hs"
    createDirectoryIfMissing True outDir
    operators <- sort . map (head . splitOn '.')
                      . filter (".hs" `isSuffixOf`)
                  <$> getDirectoryContents opDir

    let outText = unlines $ concat
            [ [ "--This is an auto-generated file created by unhaskell scripts/add_ops_metadata.hs"
              , "module Gen.Essence.Op.Internal.Generated where"
              , ""
              , "import Conjure.Language.Expression.Op"
              , "import Gen.Essence.St"
              , "import Gen.Helpers.StandardImports"
              , ""
              ]
            , sort [ (printf "import Gen.Essence.Op.%s()" m ) :: String
                | m <- operators
              ]
            , [ ""
              , ""
              , "allOps :: forall m a"
              , "        . (Generate a, MonadState St m, Applicative m)"
              , "       => GenerateConstraint"
              , "       -> [((TType -> m Bool ), (Key, GenSt (Op a)))]"
              , "allOps con = "
              , "  ["
              ]




            , [intercalate "  ,"
                [ (printf "  ( possible (error \"possible generated\" :: Op%-15s a ), (getId (error \"getId generated\" :: Op%-15s a),  MkOp%-15s <$> give con))\n" m m m) :: String
                | m <- operators ]
              ]


            , [ "  ]"

              ]
            ]

    writeOut outFile outText



writeOut :: FilePath -> String -> IO ()
writeOut outFile outText= do
  outText' <- catch (Just <$> readFile outFile)
                    (\ (_ :: IOException) -> return Nothing )
  if and [ Just (length outText) /= (length <$> outText')
         , Just outText /= outText'
         ]
      then do
          putStrLn $ "Generating " ++ outFile
          writeFile outFile outText
      else
          putStrLn $ "Reusing " ++ outFile


splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn ch (ch2:rest) | ch == ch2 = splitOn ch rest
splitOn ch rest =
  let (before, after) = span (/=ch) rest
  in  before : splitOn ch after
