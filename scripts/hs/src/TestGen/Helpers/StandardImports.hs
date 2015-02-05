{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module TestGen.Helpers.StandardImports (
      module X,
      Pretty(..)
) where

import AST.Imports as X

import Common.Helpers as X

import Common.Placeholders as X (placeholder,notImplemented,todo)

import Language.E as X

import Control.Monad as X (filterM, guard)
import Control.Monad.State.Strict as X (evalStateT,execStateT, StateT)

import Data.Default as X ( Default, def )
import Data.Set as X (Set)

import Test.QuickCheck as X (quickCheckWith, quickCheckWithResult
    , quickCheckResult, quickCheck, Gen,generate, sample'
    , Arbitrary(..), CoArbitrary(..), elements, sized)

import Text.Groom as X (groom)

import Language.E as X

import qualified Text.PrettyPrint as Pr


instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
    pretty (a,b,c) = prettyListDoc Pr.parens "," [pretty a, pretty b, pretty c]
