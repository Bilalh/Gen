module Gen.Instance.UI where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.RaceRunner
import Gen.Instance.Uniform
import Gen.IO.Formats
import Gen.Instance.Method
import Conjure.Language.Constant
import Conjure.Language.NameResolution          (resolveNames)
import Conjure.UI.IO
import Conjure.Language

import qualified Data.Set as S

-- The starting point for instance generation
runMethod :: (Sampling a, ToJSON a, MonadIO m) => LogLevel -> Method a -> m ()
runMethod lvl state= runLoggerPipeIO lvl $
       void $ flip execStateT state (createParamEssence >> run)


-- | Make the value provider for the givens
makeProvider :: (MonadIO m, MonadLog m) => FilePath -> VarInfo ->  m Provider
makeProvider fp  VarInfo{..} = do
  model <-  liftIO $ ignoreLogs $ readModelFromFile fp >>= runNameGen . resolveNames
  vs <- core model
  cons <- liftIO $ forM vs $ \(n,expr) -> do
          v <- mapM e2c expr
          return (n, v)
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
_ex_info, _ex_essence, _ex_out, _ex_mode, _ex_dir, _ex_prob :: String
_ex_prob    = "prob006-GR"
_ex_mode    = "df"
_ex_out     = "/Users/bilalh/CS/gen/__"
_ex_dir     = "/Users/bilalh/CS/instancegen-models/2015-09-23/"
_ex_info    = _ex_dir </> _ex_prob </> "info.json"
_ex_essence = _ex_dir </> _ex_prob </> _ex_prob <.> ".essence"
_ex_point  :: Point
_ex_point   = Point [("n", ConstantInt 4)] -- GR
-- _ex_point   = Point [("k", ConstantInt 2),("n", ConstantInt 4)] -- Langford
-- _ex_point = Point -- PPP
--   [(Name "capacity",
--     ConstantAbstract
--       (AbsLitFunction
--          [(ConstantInt 1, ConstantInt 3), (ConstantInt 2, ConstantInt 2),
--           (ConstantInt 3, ConstantInt 3)])),
--    (Name "crew",
--     ConstantAbstract
--       (AbsLitFunction
--          [(ConstantInt 1, ConstantInt 2), (ConstantInt 2, ConstantInt 2),
--           (ConstantInt 3, ConstantInt 1)])),
--    (Name "n_periods", ConstantInt 89),
--    (Name "n_boats", ConstantInt 3), (Name "n_upper", ConstantInt 3)]

-- Common settings
_ex_common :: IO MCommon
_ex_common = do
  i :: VarInfo <- readFromJSON _ex_info
  p <- ignoreLogs $ makeProvider _ex_essence i
  let common            = MCommon{
        mEssencePath    = _ex_essence
      , mOutputDir      = _ex_out
      , mModelTimeout   = 30
      , mVarInfo        = i
      , mPreGenerate    = Nothing
      , mIterations     = 3
      , mMode           = _ex_mode
      , mGivensProvider = p
      , mPoints         = []
      , mCores          = 2
      }

  return common


-- Run Uniform
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
