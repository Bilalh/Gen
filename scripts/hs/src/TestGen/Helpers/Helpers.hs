{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module TestGen.Helpers.Helpers where

import Language.E hiding(trace)

import Test.QuickCheck

typeOfC :: E -> E
typeOfC e  =
    let (mresult, _logs) = runCompESingle "typeOf" helper
    in case mresult of
        Right ss ->  ss
        Left d     -> error . show . vcat $  ["typeOf", d, pretty _logs]

    where
        helper = do
            typeOf e

-- _sample :: Int -> IO ()
_sample :: Pretty a => Gen a -> IO ()
_sample e  = do
    a <- sample' e
    mapM_  (putStrLn  . show . pretty) a
