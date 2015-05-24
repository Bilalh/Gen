{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

import Control.Applicative ((<$>))
import Control.Exception   (IOException, catch)
import Data.List           (isSuffixOf, sort, intercalate)
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
            [ [ "--This is an auto-generated file created by make update_ops_metadata"
              , "module Gen.Essence.Op.Internal.Generated (allOps) where"
              , ""
              , "import Conjure.Language.AdHoc"
              , "import Conjure.Language.Expression.Op"
              , "import Gen.Essence.EvalToInt"
              , "import Gen.Essence.St"
              , "import Gen.Imports"
              , ""
              ]
            , sort [ (printf "import Gen.Essence.Op.%s()" m ) :: String
                | m <- operators
              ]
            , [ ""
              , ""
              , "allOps :: forall m a"
              , "        . (Generate a, MonadState St m, Applicative m,  ExpressionLike a, EvalToInt a, WrapConstant a, MonadLog m)"
              , "       => GenerateConstraint"
              , "       -> [((GenerateConstraint -> m Bool ), (Key, GenSt (Op a)))]"
              , "allOps con = "
              , "  ["
              , intercalate "  ,"
                [ let mm :: String = printf "(Proxy :: Proxy (Op%s a))" m
                  in printf " (possible %-40s, (K_Op%-15s, MkOp%-15s <$> give con ))\n" mm m m
                | m <- operators ]
              ,  "  ]"
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
