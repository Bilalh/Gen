{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Main where

import Language.E
import Language.E.GenerateParams.Typedefs
import Language.E.Pipeline.ReadIn(readSpecPreambleFromFile)
import Language.E.GenerateParams.Groom2(groom)
import Language.E.Up.Debug(upBug, prettyAsBoth)


import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V

import System.Environment(getArgs)


main :: IO ()
main =  do
    args <- getArgs
    case args of
        [fp,outJson] -> main' fp outJson
        _    -> error "EssenceLettingsToJson <essence> <out-json>"


main' :: EssenceFP -> FilePath -> IO ()
main' fp  outJson  = do
    (Spec _ e)  <- readSpecPreambleFromFile fp
    let
        es         = statementAsList e
        parts      = map f . filter onlyLettings  $ es

    mapM_ (print . prettyAsPaths)  es
    mapM_ (print . pretty)  parts
    mapM_ (\a -> putStrLn "" >> putStrLn ( groom a)  )  parts

    let encoded = map convertToJson parts
    {-mapM  (\a -> putStrLn "" >> putStrLn ( groom a)  ) $ encoded    -}


    let result = A.object [
              "lettings" .=  (A.Array $ V.fromList encoded)
            ]

    let jsonStr    = A.encode result
    BS.writeFile outJson jsonStr
    putStrLn $ "[Conjure] Wrote " ++ outJson

    return ()


    where
    {-convertToJson :: (Text, [E]) -> Value -}
    convertToJson (name, dom) =
        A.object [ "domain" .= toJSON dom
                 , "name"   .= name
                 ]


    onlyLettings  [xMatch| _ := topLevel.letting  |] = True
    onlyLettings  _  = False

    f [xMatch|  d              := topLevel.letting.expr
             | [Prim (S name)] := topLevel.letting.name.reference |] = (name,d)

    f e = _bugg $ "f" ++ (show $ prettyAsBoth e)

_bug :: String -> [E] -> t
_bug  s = upBug  ("EssenceGivensToJson: " ++ s)
_bugg :: String -> t
_bugg s = _bug s []
