{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.Instance.BuildDependencyGraph where

import Gen.Imports
import Conjure.Language.Definition
import Data.Map(Map)
import qualified Data.Map as M

type Edge = (Name, Name)

buildDependencyGraph :: MonadLog m => Model -> m [Edge]
buildDependencyGraph model = do
  vs <- core model
  state :: [Edge] <- execWriterT $ forM vs $ \(n,dom) -> do
             let refs = [ (n,name) | (Reference name _) <- universe (Domain dom)]
             tell refs
  let edges = nub2 state

  let (l_n_i,l_i_n) = unzip $ zipWith (\(n,_) i -> ((n,i), (i,n)) )  vs [1:: Int ..]
  let n_i = M.fromList l_n_i
  let i_n = M.fromList l_i_n

  let numbedMay = map (\(fro,to) -> (fro `M.lookup` n_i, to `M.lookup` n_i)) edges
  let numbed = [ (a,b) |  (Just a, Just b) <- numbedMay ]
  unless (length numbed == length edges) $ docError [ "Edges not converted"
                                                    , vcat . map  pretty $ edges
                                                    , pretty $line]

  logInfo2 $line (map pretty numbed)


  return edges

  where
  core m = do
    vs <- forM (mStatements m) $ \ st -> case st of
      Declaration (FindOrGiven Given nm dom) ->
            return $ Just (nm, dom)
      _ -> return Nothing
    return $ catMaybes vs
