{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}


module TestGen.Classify.DomTypes where

import TestGen.Prelude

import qualified Data.Traversable as T

import qualified Data.Map as M

domTypes :: (WithDoms m) => m [TType]
domTypes = do
  (SpecE ds _ x)  <- getSpecEWithDoms
  tys <-  T.mapM ttypeOf  ds
  m <- case x of
    Nothing             -> return Nothing
    Just (Maximising m) -> fmap Just $ ttypeOf m
    Just (Minimising m) -> fmap Just $ ttypeOf m

  return . nub2 $ (map snd .  M.toList $ tys) ++ maybeToList m
