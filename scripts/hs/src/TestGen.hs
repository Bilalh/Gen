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
import Runner

import System.Process(rawSystem)
import System.FilePath((</>), (<.>))
import System.Directory(createDirectoryIfMissing)

chooseFindsDomains :: (Monad m, Applicative m, Functor m) =>  StateT GenState m ()
chooseFindsDomains = do
    levels <- rangeRandomG (1,2)
    dom :: EssenceDomain  <- pickVal (levels)

    i <- gets gFindIndex
    let name = T.pack $  "var" ++  (show  i)
    fs <- gets gFinds
    modify ( \s-> s{gFindIndex = i+1
                   ,gFinds = (name,  fromEssenceDomain dom) : fs  }  )

makeEs :: (Monad m, Applicative m, Functor m) =>  StateT GenState m  [E]
-- makeEs :: StateT GenState IO  [E]
makeEs = do
    -- aff <- mapM (\x -> rangeRandomG (1,3)) [1..3]
    -- liftIO $  print aff
    chooseFindsDomains
    gs <- gets gFinds
    return $  fmap (\(n,e) -> mkFind ((mkName n), e) ) gs


run :: IO [E]
run = do
    seed <- getStdGen
    putStrLn $ "Using seed:"  ++ show seed
    (res,st) <- runStateT makeEs GenState{gFinds=[], gFindIndex=0, genSeed=seed}
    return $ res

main :: IO ()
main = do
    es <- run

    ts <- timestamp >>= return .show

    createDirectoryIfMissing True $ "__" </> ts
    let name = ("__" </>  ts </> ts <.> ".essence")

    spec <- mkSpec es
    writeSpec name spec

    result <- runToolChain name ("__" </> ts)  6
    -- putStrLn . groom $ result
    return ()

mkSpec :: [E] -> IO Spec
mkSpec es = do

    let spec = Spec (LanguageVersion "Essence" [1,3])
         . listAsStatement
         -- . normaliseSolutionEs
         $ es
    print .  pretty $ spec
    return spec


