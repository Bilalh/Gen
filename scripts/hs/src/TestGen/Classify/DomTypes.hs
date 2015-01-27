{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}


module TestGen.Classify.DomTypes where

import TestGen.Prelude

import qualified Data.Traversable as T

import qualified Data.Map as M

domTypes :: (WithDoms m) => m [Type]
domTypes = do
  (SpecE ds _)  <- getSpecEWithDoms
  tys <-  T.mapM ttypeOf  ds
  return . nub2 . map snd .  M.toList $ tys
