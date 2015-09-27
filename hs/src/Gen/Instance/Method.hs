module Gen.Instance.Method where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.RaceRunner
import Gen.Instance.Point
import Gen.IO.Formats

run :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
    => m ()
run = do
  date_start <- timestamp

  looper 0

  date_end <- timestamp
  liftIO $ groomPrint date_start
  liftIO $ groomPrint date_end

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
randomPoint = $notDone

runParamAndStoreQuality :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
                        => Point -> m ()
runParamAndStoreQuality point = do
  (Method MCommon{mOutputDir} _) <- get
  let fp = mOutputDir </> "_params" </> pointHash point <.> ".param"
  writePoint point fp
  _ <- runRace fp
  return ()


storeDataPoint :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
               => Point -> m ()
storeDataPoint point = modify $ \(Method c@MCommon{mPoints} x) ->
                         (Method c{mPoints=point:mPoints} x)
