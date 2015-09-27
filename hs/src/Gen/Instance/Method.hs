module Gen.Instance.Method where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.RaceRunner
import Gen.Instance.Point
import Gen.IO.Formats

run :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m, ToJSON a)
    => m ()
run = do
  date_start <- timestamp

  looper 0

  date_end <- timestamp
  liftIO $ groomPrint date_start
  liftIO $ groomPrint date_end

  st@(Method MCommon{mOutputDir} _) <- get
  liftIO $ writeToJSON (mOutputDir </> "state.json") st


looper :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
       => Int -> m ()
looper i = do
  (Method MCommon{mIterations} _) <- get
  if i == mIterations then
      return ()
  else
    doIteration >>= \case
      SamplingSuccess -> looper (i + 1)
      _               -> looper (i)



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
