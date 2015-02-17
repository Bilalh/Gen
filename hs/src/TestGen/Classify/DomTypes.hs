{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}


module TestGen.Classify.DomTypes where

import TestGen.Prelude

import qualified Data.Traversable as T

import qualified Data.Map as M

domTypes :: (WithDoms m) => m [TType]
domTypes = do
  (Spec ds _ x)  <- getSpecEWithDoms
  tys <-  T.mapM ttypeOf  ds
  m <- case x of
    Nothing             -> return Nothing
    Just (Maximisingg m) -> fmap Just $ ttypeOf m
    Just (Minimisingg m) -> fmap Just $ ttypeOf m

  return . nub2 $ (map snd .  M.toList $ tys) ++ maybeToList m
