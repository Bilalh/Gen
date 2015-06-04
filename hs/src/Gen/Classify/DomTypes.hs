module Gen.Classify.DomTypes where

import Gen.Imports
import Gen.Helpers.TypeOf

import qualified Data.Traversable as T
import qualified Data.Map as M

domTypes :: (Monad m, Applicative m) => Spec ->  m [Type]
domTypes (Spec ds _ _) = do
  tys <-  T.mapM ttypeOf  ds
  return . nub2 $ (map snd .  M.toList $ tys)
