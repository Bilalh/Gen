--{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE ConstraintKinds #-}

module Main where

import Helpers
import Test
import Data
import Runner

import Language.E
import Language.E.Pipeline.ReadIn(writeSpec)

import Control.Monad.Trans.State.Strict(StateT)

import Data.Set(Set)

import System.Directory(createDirectoryIfMissing)
import System.FilePath((</>), (<.>))
import System.Process(rawSystem)
import System.Random(randomIO,mkStdGen)

import Text.Groom(groom)

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Test.QuickCheck as Q

chooseFindsDomain :: MonadGen m => m ()
chooseFindsDomain = do
    levels <- rangeRandomG (1,2)
    dom :: EssenceDomain  <- pickVal (levels)

    i <- gets gFindIndex
    let name = T.pack $  "var" ++  (show  i)
    fs <- gets gFinds
    modify ( \s-> s{gFindIndex = i+1
                   ,gFinds = (name,  fromEssenceDomain dom) : fs  }  )
    return ()

makeEs :: MonadGen m => m [E]
makeEs = do
    varsNum <- rangeRandomG (1,3)
    mapM_ (\_ -> chooseFindsDomain) [1..varsNum]
    gs <- gets gFinds
    return $  fmap (\(n,e) -> mkFind ((mkName n), e) ) gs


run :: StdGen -> Float -> IO ()
run _ limit | limit <= 0  = return ()
run seed limit = do
    putStrLn $ show limit ++ " seconds left"
    (es,st) <- runStateT makeEs GenState{gFinds=[], gFindIndex=0, genSeed=seed}

    ts <- timestamp >>= return .show
    createDirectoryIfMissing True $ "__" </> ts
    let name = ("__" </>  ts </> ts <.> ".essence")
    spec <- mkSpec es
    writeSpec name spec
    result <- runToolChain name ("__" </> ts)  6

    run (genSeed st) (limit - total_cpu_time result)

main :: IO ()
main = do
    seedd :: Int <- randomIO
    let seed = mkStdGen seedd
    -- seed <- getStdGen
    putStrLn $ "Using seed:"  ++ show seed
    run seed 12

mkSpec :: [E] -> IO Spec
mkSpec es = do

    let spec = Spec (LanguageVersion "Essence" [1,3])
         . listAsStatement
         -- . normaliseSolutionEs
         $ es
    print .  pretty $ spec
    return spec


