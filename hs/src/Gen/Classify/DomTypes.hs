module Gen.Classify.DomTypes where

import Gen.Imports
import Gen.Helpers.TypeOf

import qualified Data.Map as M

domTypes :: (Monad m, Applicative m) => Spec ->  m [Type]
domTypes (Spec ds _ _) = do
  tys <-  mapM (ttypeOf . snd)  ds
  return . nub2 $ (map snd .  M.toList $ tys)
