module Gen.Instance.UI where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.RaceRunner
import Gen.Instance.Uniform
import Gen.IO.Formats
import Gen.Instance.Method
import Gen.Instance.Point
import Conjure.Language.Constant
import Conjure.Language.NameResolution          (resolveNames)
import Conjure.UI.IO
import Conjure.Language

import qualified Data.Set as S

-- | Make the value provider for the givens
makeProvider :: MonadIO m => FilePath -> VarInfo ->  m Provider
makeProvider fp  VarInfo{..} = do
  model <-  liftIO $ ignoreLogs $ readModelFromFile fp >>= runNameGen . resolveNames
  vs <- ignoreLogs $ core model
  let cons = (flip map) vs $ \(n,expr) ->
          case e2c (Domain expr) of
            Just (DomainInConstant x) -> (n,x)
            _ -> error "Not a constant Domain"
  return $ Provider cons

  where
  core m = do
    vs <- forM (mStatements m) $ \ st -> case st of
      Declaration (FindOrGiven Given nm@(Name te) dom) ->
            if te `S.member` givens
            then return $ Just (nm, dom)
            else return $ Nothing
      _ -> return Nothing

    return $ catMaybes vs


-- for examples
_ex_info, _ex_essence, _ex_out, _ex_mode :: String
_ex_info    = "/Users/bilalh/CS/instancegen-models/_current/prob006-GR/info.json"
_ex_essence = "/Users/bilalh/CS/instancegen-models/_current/prob006-GR/prob006-GR.essence"
_ex_out     = "/Users/bilalh/CS/gen/__"
_ex_mode    = "df"
_ex_point  :: Point
_ex_point   = Point [("n", ConstantInt 4)]

-- Common settings
_ex_common :: IO MCommon
_ex_common = do
  i :: VarInfo <- readFromJSON _ex_info
  p <- makeProvider _ex_essence i
  let common            = MCommon{
        mEssencePath    = _ex_essence
      , mOutputDir      = _ex_out
      , mModelTimeout   = 30
      , mVarInfo        = i
      , mPreGenerate    = Nothing
      , mIterations     = 1
      , mMode           = _ex_mode
      , mGivensProvider = p
      }

  return common


-- Run Uniform once
_ex8 :: IO ()
_ex8 = do
  common <- _ex_common
  let state = Method common Uniform

  let workload = runLoggerPipeIO (LogDebug) $ do
        (re,reState) <- runStateT (run) state
        logInfo "Finished"
        liftIO $  groomPrint re
        liftIO $  groomPrint reState

  workload


-- Run a race and collect results
_ex7  :: IO ()
_ex7 = do
  common <- _ex_common
  let state = Method common Uniform

  let workload = runLoggerPipeIO (LogDebug) $ do
        (re,reState) <- runStateT (runParamAndStoreQuality _ex_point) state
        logInfo "Finished"
        liftIO $  groomPrint re
        liftIO $  groomPrint reState


  workload


-- Sampling using minion
_ex5  :: IO ()
_ex5 = do
  common <- _ex_common
  let state = Method common Uniform

  let workload = runLoggerPipeIO (LogDebug) $ do
        (re,reState) <- runStateT sampleParamFromMinion state
        logInfo "Finished"
        liftIO $  groomPrint re
        liftIO $  groomPrint reState


  workload


-- Creating the param essence
_ex4  :: IO ()
_ex4 = do
  common <- _ex_common
  let state = Method common Uniform

  let workload = runLoggerPipeIO (LogDebug) $ do
        (re,reState) <- runStateT createParamEssence state
        logInfo "Finished"
        liftIO $  groomPrint re
        liftIO $  groomPrint reState


  workload
