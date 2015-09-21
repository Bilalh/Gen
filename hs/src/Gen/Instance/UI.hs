module Gen.Instance.UI where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.RaceRunner
import Gen.Instance.Uniform()
import Gen.IO.Formats

_ex4 = do
  i :: VarInfo <- readFromJSON "/Users/bilalh/CS/instancegen-models/_new/prob013-PPP/info.json"
  let common = MCommon{
        mEssencePath = "/Users/bilalh/CS/instancegen-models/prob013-PPP/prob013-PPP.essence"
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


-- _ex3 = do
--   i :: VarInfo <- readFromJSON "/Users/bilalh/CS/instancegen-models/_new/prob013-PPP/info.json"
--   let common = MCommon{
--         mEssencePath = "/Users/bilalh/CS/instancegen-models/prob013-PPP/prob013-PPP.essence"
--       , mOutputDir   = "/Users/bilalh/CS/gen/__"
--       , mRaceTimeout = "60"
--       , mVarInfo     = i
--       , mPreGenerate = Nothing
--       , mIterations  = 1
--       }
--   let state = Method common Uniform
--   (re,reState) <- runStateT createParamEssence state
--   groomPrint re
--   groomPrint reState

_ex4 :: IO ()
