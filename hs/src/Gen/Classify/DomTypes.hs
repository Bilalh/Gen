



module Gen.Classify.DomTypes where

import Gen.Prelude

import qualified Data.Traversable as T

import qualified Data.Map as M

domTypes :: (WithDoms m) => m [Type]
domTypes = do
  (Spec ds _ _)  <- getSpecEWithDoms
  tys <-  T.mapM ttypeOf  ds
  -- m <- case x of
  --   Nothing     -> return Nothing
  --   Just (_, e) -> fmap Just $ ttypeOf e

  -- return . nub2 $ (map snd .  M.toList $ tys) ++ maybeToList m
  return . nub2 $ (map snd .  M.toList $ tys)
