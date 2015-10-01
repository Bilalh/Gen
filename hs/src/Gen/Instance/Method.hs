module Gen.Instance.Method where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.RaceRunner
import Gen.Instance.Point
import Gen.IO.Formats
import System.Random(setStdGen, mkStdGen)
import System.CPUTime ( getCPUTime )


-- | The main instance generation,
run :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m, ToJSON a)
    => m ()
run = do
  startOurCPU <- liftIO $  getCPUTime

  liftIO $ setStdGen (mkStdGen 33)

  rTimestampStart <- timestamp

  (rIterationsDone, rIterationsDoneIncludingFailed) <- looper 0 0

  rTimestampEnd <- timestamp

  st@(Method MCommon{mOutputDir, mPoints} _) <- get
  liftIO $ writeToJSON (mOutputDir </> "state.json") st
  liftIO $ writeToJSON (mOutputDir </> "points.json") mPoints

  subCPU <- subprocessTotalCpuTime
  endOurCPU <- liftIO $ getCPUTime
  let ourCPU = fromIntegral (endOurCPU - startOurCPU) / ((10 :: Double) ^ (12 :: Int))


  let meta = RunMetadata
        { rTimestampStart
        , rTimestampEnd
        , rRealTime   = rTimestampEnd - rTimestampStart
        , rCPUTime    = subCPU + ourCPU
        , rSubCPUTime = subCPU
        , rOurCPUTime = ourCPU
        , rIterationsDone
        , rIterationsDoneIncludingFailed
        }

  liftIO $ groomPrint meta
  liftIO $ writeToJSON (mOutputDir </> "metadata.json") st


looper :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
       => Int -> Int -> m (Int,Int)
looper i j= do
  (Method MCommon{mIterations} _) <- get
  if i == mIterations then
      return (i,j)
  else
    doIteration >>= \case
      SamplingSuccess{} -> looper (i + 1) (j + 1)
      x               -> do
        logInfo2 $line ["Not counting iteration because of" <+> pretty x ]
        looper i (j + 1)



randomPoint :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
            =>  m Point
randomPoint = sampleParamFromMinion

runParamAndStoreQuality :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
                        => Point -> m ()
runParamAndStoreQuality point = do
  let h =pointHash point
  checkPrevious h >>= \case
    Just _  -> return ()
    Nothing -> do
      (Method MCommon{mOutputDir} _) <- get
      let fp = mOutputDir </> "_params" </> h <.> ".param"
      writePoint point fp
      _ <- runRace fp
      return ()


storeDataPoint :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
               => Point -> m ()
storeDataPoint point = modify $ \(Method c@MCommon{mPoints} x) ->
                         (Method c{mPoints=point:mPoints} x)
