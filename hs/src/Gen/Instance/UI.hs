module Gen.Instance.UI where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.RaceRunner
import Gen.Instance.Uniform
import Gen.IO.Formats
import Gen.Instance.Method
import Gen.Instance.Point
import Conjure.Language.Constant

-- for examples
_ex_info, _ex_essence, _ex_out, _ex_mode :: String
_ex_info    = "/Users/bilalh/CS/instancegen-models/_current/prob006-GR/info.json"
_ex_essence = "/Users/bilalh/CS/instancegen-models/_current/prob006-GR/prob006-GR.essence"
_ex_out     = "/Users/bilalh/CS/gen/__"
_ex_mode    = "df"
_ex_point  :: Point
_ex_point   = Point [("n", ConstantInt 4)]

-- Run Uniform once
_ex8 :: IO ()
_ex8 = do
  i :: VarInfo <- readFromJSON _ex_info
  let common = MCommon{
        mEssencePath  = _ex_essence
      , mOutputDir    = _ex_out
      , mModelTimeout = 30
      , mVarInfo      = i
      , mPreGenerate  = Nothing
      , mIterations   = 1
      , mMode         = _ex_mode
      }
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
  i :: VarInfo <- readFromJSON _ex_info
  let common = MCommon{
        mEssencePath  = _ex_essence
      , mOutputDir    = _ex_out
      , mModelTimeout = 30
      , mVarInfo      = i
      , mPreGenerate  = Nothing
      , mIterations   = 1
      , mMode         = _ex_mode
      }
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
  i :: VarInfo <- readFromJSON _ex_info
  let common = MCommon{
        mEssencePath  = _ex_essence
      , mOutputDir    = _ex_out
      , mModelTimeout = 60
      , mVarInfo      = i
      , mPreGenerate  = Nothing
      , mIterations   = 1
      , mMode         = _ex_mode
      }
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
  i :: VarInfo <- readFromJSON _ex_info
  let common = MCommon{
        mEssencePath  = _ex_essence
      , mOutputDir    = _ex_out
      , mModelTimeout = 60
      , mVarInfo      = i
      , mPreGenerate  = Nothing
      , mIterations   = 1
      , mMode         = _ex_mode
      }
  let state = Method common Uniform

  let workload = runLoggerPipeIO (LogDebug) $ do
        (re,reState) <- runStateT createParamEssence state
        logInfo "Finished"
        liftIO $  groomPrint re
        liftIO $  groomPrint reState


  workload
