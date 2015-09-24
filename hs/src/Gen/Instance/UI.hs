module Gen.Instance.UI where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.RaceRunner
import Gen.Instance.Uniform()
import Gen.IO.Formats


-- Runn a race and collect results
_ex7  :: IO ()
_ex7 = do
  i :: VarInfo <- readFromJSON "/Users/bilalh/CS/instancegen-models/_current/prob006-GR/info.json"
  let common = MCommon{
        mEssencePath = "/Users/bilalh/CS/instancegen-models/_current/prob006-GR/prob006-GR.essence"
      , mOutputDir   = "/Users/bilalh/CS/gen/__"
      , mModelTimeout = 30
      , mVarInfo     = i
      , mPreGenerate = Nothing
      , mIterations  = 1
      }
  let state = Method common Uniform

  let paramPath = "/Users/bilalh/CS/gen/__/_params/test.param"

  let workload = runLoggerPipeIO (LogDebug) $ do
        (re,reState) <- runStateT (runRace paramPath) state
        logInfo "Finished"
        liftIO $  groomPrint re
        liftIO $  groomPrint reState


  workload



-- do a race
_ex6  :: IO ()
_ex6 = do
  i :: VarInfo <- readFromJSON "/Users/bilalh/CS/instancegen-models/_current/prob006-GR/info.json"
  let common = MCommon{
        mEssencePath = "/Users/bilalh/CS/instancegen-models/_current/prob006-GR/prob006-GR.essence"
      , mOutputDir   = "/Users/bilalh/CS/gen/__"
      , mModelTimeout = 30
      , mVarInfo     = i
      , mPreGenerate = Nothing
      , mIterations  = 1
      }
  let state = Method common Uniform

  let paramPath = "/Users/bilalh/CS/gen/__/_params/test.param"

  let workload = runLoggerPipeIO (LogDebug) $ do
        (re,reState) <- runStateT (doRace paramPath) state
        logInfo "Finished"
        liftIO $  groomPrint re
        liftIO $  groomPrint reState


  workload


-- Sampling using minion
_ex5  :: IO ()
_ex5 = do
  i :: VarInfo <- readFromJSON "/Users/bilalh/CS/instancegen-models/_current/prob006-GR/info.json"
  let common = MCommon{
        mEssencePath = "/Users/bilalh/CS/instancegen-models/_current/prob006-GR/prob006-GR.essence"
      , mOutputDir   = "/Users/bilalh/CS/gen/__"
      , mModelTimeout = 60
      , mVarInfo     = i
      , mPreGenerate = Nothing
      , mIterations  = 1
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
  i :: VarInfo <- readFromJSON "/Users/bilalh/CS/instancegen-models/_current/prob006-GR/info.json"
  let common = MCommon{
        mEssencePath = "/Users/bilalh/CS/instancegen-models/_current/prob006-GR/prob006-GR.essence"
      , mOutputDir   = "/Users/bilalh/CS/gen/__"
      , mModelTimeout = 60
      , mVarInfo     = i
      , mPreGenerate = Nothing
      , mIterations  = 1
      }
  let state = Method common Uniform

  let workload = runLoggerPipeIO (LogDebug) $ do
        (re,reState) <- runStateT createParamEssence state
        logInfo "Finished"
        liftIO $  groomPrint re
        liftIO $  groomPrint reState


  workload
