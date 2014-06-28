{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test where

import Language.E hiding(EssenceLiteral(..))
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
import Data

a :: Int -> IO Int
a f = return 44

b :: Int -> IO Int
b f = return $ f + 1

dd :: IO Int
dd =  return
    >=> a
    >=> b
     $  4


