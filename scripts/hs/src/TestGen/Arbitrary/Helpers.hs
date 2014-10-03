{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module TestGen.Arbitrary.Helpers where

import AST.Imports
import TestGen.Arbitrary.Type
import Language.E hiding(trace)

import Test.QuickCheck


-- | Generates a random length between the specifed bounds.
--   The length depends on the size parameter.
listOfB :: Int -> Int -> Gen a -> Gen [a]
listOfB l u gen = sized $ \n -> do
    k <- choose ( 0 `max` l, u `min` n)
    vectorOf k gen


-- TODO could be a lot more efficient
typesUnifyO  :: Domain -> Domain -> Bool
typesUnifyO a b = typeUnify (toEssence a) (toEssence b)



typeOfC :: E -> E
typeOfC e  =
    let (mresult, _logs) = runCompESingle "typeOf" helper
    in case mresult of
        Right ss ->  ss
        Left d     -> error . show .  vcat $ ["typeOf", d, pretty _logs]

    where
        helper = do
            typeOf e

-- _sample :: Int -> IO ()
_sample :: Pretty a => Gen a -> IO ()
_sample e  = do
    a <- sample' e
    mapM_  (putStrLn  . show . pretty) a
