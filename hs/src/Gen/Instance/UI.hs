module Gen.Instance.UI where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.RaceRunner
import Gen.Instance.Uniform
import Gen.IO.Formats
import Gen.Instance.Method

-- FilePaths for examples
_ex_info, _ex_essence, _ex_out, _ex_param :: FilePath
_ex_info    = "/Users/bilalh/CS/instancegen-models/_current/prob006-GR/info.json"
_ex_essence = "/Users/bilalh/CS/instancegen-models/_current/prob006-GR/prob006-GR.essence"
_ex_out     = "/Users/bilalh/CS/gen/__"
_ex_param   = "/Users/bilalh/CS/gen/__/_params/test.param"

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
      }
  let state = Method common Uniform

  let workload = runLoggerPipeIO (LogDebug) $ do
        (re,reState) <- runStateT (runRace _ex_param) state
        logInfo "Finished"
        liftIO $  groomPrint re
        liftIO $  groomPrint reState


  workload


-- do a race
_ex6  :: IO ()
_ex6 = do
  i :: VarInfo <- readFromJSON _ex_info
  let common = MCommon{
        mEssencePath  = _ex_essence
      , mOutputDir    = _ex_out
      , mModelTimeout = 30
      , mVarInfo      = i
      , mPreGenerate  = Nothing
      , mIterations   = 1
      }
  let state = Method common Uniform


  let workload = runLoggerPipeIO (LogDebug) $ do
        (re,reState) <- runStateT (doRace _ex_param) state
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
      }
  let state = Method common Uniform

  let workload = runLoggerPipeIO (LogDebug) $ do
        (re,reState) <- runStateT createParamEssence state
        logInfo "Finished"
        liftIO $  groomPrint re
        liftIO $  groomPrint reState


  workload
