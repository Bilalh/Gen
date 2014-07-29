{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module TestGen.Test where


import Helpers
import Data

import Language.E hiding(EssenceLiteral(..))
-- import Language.E.NormaliseSolution(normaliseSolutionEs)
import Language.E.Pipeline.ReadIn(writeSpec)

import Control.Monad.Trans.State.Strict(StateT)
import Data.DeriveTH
import Data.Set(Set)
import Test.QuickCheck
import Text.Groom(groom)

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Test.QuickCheck as Q

a :: Int -> IO Int
a f = return 44

b :: Int -> IO Int
b f = return $ f + 1

dd :: IO Int
dd =  return
    >=> a
    >=> b
     $  4



ss :: MonadGen m => m Int
ss = do
    return 4

fff :: MonadState GenGlobal m => m String
fff = do
    return "dd"

ttt :: MonadGG m =>  StateT GenState m [Int]
ttt = do
    ff <- gets eFindIndex
    hh <- lift $ gets gSeed
    aa <- ss
    f<- lift fff
    return [2 :: Int]
