{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.Instance.BuildDependencyGraph where

import Gen.Imports
import Conjure.Language.Definition
import Gen.Instance.Data
import Gen.Instance.RaceRunner (runSolve, script_lookup1,conjureCompact)
import Gen.Instance.SamplingError

import qualified Data.Map as M
import qualified Data.Set   as S

type Edge = (Name, Name)

buildDependencyGraph :: (MonadLog m, MonadIO m)
                     => FilePath -> Model
                     -> m (Either SamplingErr (VarInfo, Double))
buildDependencyGraph outBase model = do
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

  logDebugVerbose2 $line (map pretty numbed)
  let edgesSet = ConstantAbstract (AbsLitSet
       [ ConstantAbstract (AbsLitTuple [ ConstantInt (fromIntegral from)
                                       , ConstantInt (fromIntegral to)])
       |  (from,to) <- numbed
       ])

  let point = Point [("N", ConstantInt (genericLength vs)), ("Edges", edgesSet)]
  logWarn2 $line [nn "point" point]

  ess <- liftIO $ script_lookup1 "instances/find_generation_order/find_generation_order.essence"
  let eprime = outBase </> "find_generation_order.eprime"
  conjureCompact ess eprime >>= \case
    False -> return $ Left $ ErrFailedRunSolve ("failed to refine" <+> pretty ess )
    True  -> do
      runSolve outBase ess eprime point >>= \case
        Left x  -> return (Left x)
        Right (solPoint, time) -> do
          logInfo2 $line [nn "solution Point" solPoint]
          return $ Right $ (VarInfo{givens=S.empty}, time)

  where
  core m = do
    vs <- forM (mStatements m) $ \ st -> case st of
      Declaration (FindOrGiven Given nm dom) ->
            return $ Just (nm, dom)
      _ -> return Nothing
    return $ catMaybes vs