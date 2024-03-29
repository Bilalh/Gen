{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.Instance.BuildDependencyGraph where

import Gen.Imports
import Conjure.Language.Definition
import Gen.Instance.Data
import Gen.Instance.Point
import Gen.Instance.RaceRunner (runSolve, script_lookup1,conjureCompact)
import Gen.Instance.SamplingError
import Gen.Helpers.InlineLettings
import Conjure.Language.NameResolution (resolveNames)

import qualified Data.Map as M
import qualified Data.Set   as S

type Edge = (Name, Name)

buildDependencyGraph :: (MonadLog m, MonadIO m)
                     => FilePath -> Model
                     -> m (Either SamplingErr (VarInfo, Double))
buildDependencyGraph outBase modelStart = do
  modelNamed <- liftIO $ ignoreLogs . runNameGen $ resolveNames modelStart
  let model = inlineLettings modelNamed
  vs <- core model
  state :: [Edge] <- execWriterT $ forM vs $ \(n,dom) -> do
             let refs = [ (n,name) | (Reference name _) <- universe (Domain dom)]
             tell refs
  let edges = nub2 state
  logDebugVerbose2 $line (map pretty edges)

  let (l_n_i,l_i_n) = unzip $ zipWith (\(n,_) i -> ((n,i), (i,n)) )  vs [1:: Integer ..]
  let n_i = M.fromList l_n_i
  let i_n = M.fromList (map (first ConstantInt)  l_i_n)

  let numbedMay = map (\(fro,to) -> (fro `M.lookup` n_i, to `M.lookup` n_i)) edges
  let numbed = [ (a,b) |  (Just a, Just b) <- numbedMay ]
  unless (length numbed == length edges) $ docError [ "Edges not converted, length numbed /= length edges"
                                                    , "edges"  <+>  (vcat . map  pretty $ edges)
                                                    , "numbed" <+>  (vcat . map  pretty $ numbed)
                                                    , "state"  <+> (vcat . map  pretty $ state)
                                                    , "vs"  <+>(vcat . map  pretty $ vs)
                                                    , pretty $line]

  -- logDebugVerbose2 $line (map pretty numbed)
  let edgesSet = ConstantAbstract (AbsLitSet
       [ ConstantAbstract (AbsLitTuple [ ConstantInt (fromIntegral from)
                                       , ConstantInt (fromIntegral to)])
       |  (from,to) <- numbed
       ])

  let point = Point [("N", ConstantInt (genericLength vs)), ("Edges", edgesSet)]
  logWarn2 $line [nn "point" point]

  -- let namedMay x = map (\(fro,to) -> (fro `M.lookup` i_n, to `M.lookup` i_n)) x
  -- let named x = [ (a,b) |  (Just a, Just b) <- namedMay x ]

  let namedMay x = map (\(to) -> (to `M.lookup` i_n)) x
  let named x = [ b |  (Just (Name b)) <- namedMay x ]

  ess <- liftIO $ script_lookup1 "instances/find_generation_order/find_generation_order.essence"
  let eprime = outBase </> "find_generation_order.eprime"
  conjureCompact ess eprime >>= \case
    False -> return $ Left $ ErrFailedRunSolve (nn "failed to refine" (groom ess) )
    True  -> do
      runSolve outBase ess eprime point >>= \case
        Right (solPoint@(Just (Point xs)), time) -> do
          logInfo2 $line ["solution Point", pretty solPoint]
          case (lookup "levelsNeeded" xs, lookup "levels" xs) of
            (Just l, Just (ConstantAbstract (AbsLitFunction lvls ))) ->
              case lvls of
                []  -> return $ Left $ ErrFailedRunSolve (nn "No variables" (solPoint))
                [_] -> return $ Right $ (VarInfo{givens=S.fromList []}, time)
                [(_, ConstantAbstract  (AbsLitSet as) ), _ ] -> do
                  let ns =  named as
                  return $ Right $ (VarInfo{givens=S.fromList $ ns}, time)

                _   -> return $ Left $ ErrFailedRunSolve $ vcat [
                            nn "Levels needs to be <2, currently at" l
                          , nn "Point" (solPoint) ]

            _ -> return $ Left $ ErrFailedRunSolve (nn "failed to Read" (solPoint))
        Left x  -> return (Left x)
        Right _ -> return $ Left $ ErrFailedRunSolve (nn "No solution for " point)

  where


  core m = do
    vs <- forM (mStatements m) $ \ st -> case st of
      Declaration (FindOrGiven Given nm dom) ->
            return $ Just (nm, dom)
      _ -> return Nothing
    return $ catMaybes vs
