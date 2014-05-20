{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Main where

import Language.E
import Language.E.GenerateParams.Typedefs
import Language.E.Pipeline.ReadIn(readSpecPreambleFromFile)
import Language.E.GenerateParams.Groom2(groom)
import Language.E.Pipeline.InlineLettings(inlineLettings)
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
        [fp,outJson, unboundInt] -> main' fp outJson (read unboundInt :: Integer)
        _    -> error "<essence> <out-json> <unbound-int-replacement>"


main' :: EssenceFP -> FilePath -> Integer  -> IO ()
main' fp  outJson _ = do
    spec  <- readSpecPreambleFromFile fp
    let (Spec _ e) = inlineSpec spec
        es         = statementAsList e
        parts      = map f . filter onlyGivens  $ es

    mapM_ (print . pretty)  parts
    mapM_ (\a -> putStrLn "" >> putStrLn ( groom a)  )  parts    

    let encoded = map convertToJson parts
    {-mapM  (\a -> putStrLn "" >> putStrLn ( groom a)  ) $ encoded    -}


    let result = A.object [ 
              "givens" .=  (A.Array $ V.fromList encoded)
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


    onlyGivens  [xMatch| _ := topLevel.declaration.given  |] = True
    onlyGivens  _  = False

    f [xMatch|  d              := topLevel.declaration.given.domain
             | [Prim (S name)] := topLevel.declaration.given.name.reference |] = (name,d)

    f [xMatch|  _              := topLevel.declaration.given.typeInt
             | [Prim (S name)] := topLevel.declaration.given.name.reference |] = (name,[[xMake| domain.typeInt := [] |]] )



    f e@[xMatch| [Prim (S name)] := topLevel.declaration.given.name.reference |] =  
        _bugg $ "f " ++ show name ++ (show $ prettyAsBoth e)

    f e = _bugg $ "f" ++ (show $ prettyAsBoth e)


inlineSpec :: Spec -> Spec
inlineSpec spec =
    let
        (mresult, _logs) = runCompESingle "inlining lettings" helper
    in
        case mresult of
            Left  x      -> error $ renderNormal x
            Right result -> result

    where
    helper :: FunkySingle ConjureState ConjureError Identity Spec
    helper = do
        let pipeline = recordSpec "init" 
                >=> inlineLettings >=> recordSpec "inlineLettings"
        pipeline spec


_bug :: String -> [E] -> t
_bug  s = upBug  ("EssenceLettingsToJson: " ++ s)
_bugg :: String -> t
_bugg s = _bug s []
