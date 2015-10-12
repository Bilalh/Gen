{-# LANGUAGE MultiParamTypeClasses #-}
module Gen.Instance.Method where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.RaceRunner
import Gen.Instance.Point
import Gen.IO.Formats
import System.CPUTime ( getCPUTime )
import Gen.Instance.SamplingError

-- | The main instance generation,
run :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m, ToJSON a)
    => m ()
run = do
  startOurCPU <- liftIO $  getCPUTime

  rTimestampStart <- timestamp

  (rIterationsDone, rIterationsDoneIncludingFailed) <- looper 0 0

  rTimestampEnd <- timestamp

  st@(Method MCommon{mOutputDir, mPoints, mSubCpu=rSubCPUTime} _) <- get
  liftIO $ writeToJSON (mOutputDir </> "state.json") st
  liftIO $ writeToJSON (mOutputDir </> "points.json") mPoints

  rRacesCPUTime    <- racesTotalCpuTime
  rParamGenCPUTime <- paramGenCpuTime
  endOurCPU        <- liftIO $ getCPUTime

  let rOurCPUTime = fromIntegral (endOurCPU - startOurCPU) / ((10 :: Double) ^ (12 :: Int))
  let rCPUTime    = rRacesCPUTime + rParamGenCPUTime +  rOurCPUTime  + rSubCPUTime
  let rRealTime   = rTimestampEnd - rTimestampStart

  let meta = RunMetadata
        { rTimestampStart
        , rTimestampEnd
        , rRealTime
        , rCPUTime
        , rRacesCPUTime
        , rParamGenCPUTime
        , rSubCPUTime
        , rOurCPUTime
        , rIterationsDone
        , rIterationsDoneIncludingFailed
        }

  liftIO $ groomPrint meta
  liftIO $ writeToJSON (mOutputDir </> "metadata.json") meta



looper :: forall m a . (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
       => Int -> Int -> m (Int,Int)
looper i j= do
  (Method MCommon{mIterations} _) <- get
  if i == mIterations then
      return (i,j)
  else do
    doIteration >>= \case
      Right{} -> do
         logInfo2 $line ["no error on iteration " <+> pretty (i,j)]
         looper (i + 1) (j + 1)
      Left (ErrDontCountIteration d) -> do
         logInfo2 $line ["REJECTED Not counting iteration because of" <+> pretty d ]
         looper i (j + 1)
      Left (x) -> do
         docError ["Error because of ", pretty x]


randomPoint :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
            =>  m (Either SamplingErr Point)
randomPoint = sampleParamFromMinion

runParamAndStoreQuality :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
                        => Point -> m (Either SamplingErr ())
runParamAndStoreQuality point = do
  let h =pointHash point
  checkPrevious h >>= \case
    Just _  -> return $ Right ()
    Nothing -> do
      (Method MCommon{mOutputDir} _) <- get
      let fp = mOutputDir </> "_params" </> h <.> ".param"
      writePoint point fp
      voidRes $ runRace fp


storeDataPoint :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
               => Point -> m ()
storeDataPoint point = modify $ \(Method c@MCommon{mPoints} x) ->
                         (Method c{mPoints=point:mPoints} x)



class MonadState (Method a) m => Test a m where
    doTest :: (MonadIO m,  MonadLog m, MonadSamplingFail m) => m a


-- tester :: forall a m .  (Test a m, MonadIO m, MonadLog m)
--        => Int -> Int -> m (Int,Int)
-- tester i j= do
--   (Method MCommon{mIterations} _) <- get
--   if i == mIterations then
--       return (i,j)
--   else do
--     x <- runErrT doTest
--     case x of
--       Right (c :: a) -> tester (i + 1) (j + 1)
--       Left x -> error "d"
