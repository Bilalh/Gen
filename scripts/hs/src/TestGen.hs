{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

-- import Language.E hiding(EssenceLiteral(..))
import Language.E
-- import Language.E.NormaliseSolution(normaliseSolutionEs)
import Language.E.Pipeline.ReadIn(writeSpec)

import Data.Set(Set)
import qualified Data.Set as S
import qualified Data.Text as T

import Control.Monad.Trans.State.Strict(StateT)
import Text.Groom(groom)

import Test.QuickCheck
import qualified Test.QuickCheck as Q

import Data.DeriveTH

import Helpers
import Test
import Data




chooseFindsDomains :: Monad m =>  StateT GenState m ()
chooseFindsDomains = do
    lit :: EssenceLiteral <- pickVal 1

    i <- gets gFindIndex
    let name = T.pack $  "var" ++  (show  i)
    fs <- gets gFinds
    modify ( \s-> s{gFindIndex = i+1
                   ,gFinds = (name,  fromEssenceLiteral lit) : fs  }  )

makeEs :: Monad m =>  StateT GenState m  [E]
makeEs = do
    chooseFindsDomains

    let chosenDoms = [ ("AA", [dMake| int(1..4) |]) ]

    return $  fmap (\(n,e) -> mkFind ((mkName n), e) ) chosenDoms


run :: IO [E]
run = do
    --let finds = [mkFind  ( mkName "d", [dMake| int(1..2) |] )]
    (res,st) <- runStateT makeEs GenState{gFinds=[], gFindIndex=0}
    return $ res

main :: IO ()
main = do
    es <- run

    spec <- mkSpec es
    writeSpec "a.essence" spec


-- _r pathInp = do
--     inp <- readSpecFromFile pathInp
--     let a = (runCompE "TestGen" $ _ inp)
--     return ()


mkSpec :: [E] -> IO Spec
mkSpec es = do

    let spec = Spec (LanguageVersion "Essence" [1,3])
         . listAsStatement
         -- . normaliseSolutionEs
         $ es
    print .  pretty $ spec
    return spec


