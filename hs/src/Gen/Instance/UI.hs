module Gen.Instance.UI where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.RaceRunner
import Gen.Instance.Uniform()
import Gen.IO.Formats


_ex5  :: IO ()
_ex5 = do
  i :: VarInfo <- readFromJSON "/Users/bilalh/CS/instancegen-models/_new/prob006-GR/info.json"
  let common = MCommon{
        mEssencePath = "/Users/bilalh/CS/instancegen-models/prob006-GR/prob006-GR.essence"
      , mOutputDir   = "/Users/bilalh/CS/gen/__"
      , mRaceTimeout = "60"
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



_ex4  :: IO ()
_ex4 = do
  i :: VarInfo <- readFromJSON "/Users/bilalh/CS/instancegen-models/_new/prob006-GR/info.json"
  let common = MCommon{
        mEssencePath = "/Users/bilalh/CS/instancegen-models/prob006-GR/prob006-GR.essence"
      , mOutputDir   = "/Users/bilalh/CS/gen/__"
      , mRaceTimeout = "60"
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
