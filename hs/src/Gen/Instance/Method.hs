module Gen.Instance.Method where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.RaceRunner
import Gen.Instance.Value
import Gen.IO.Formats

run :: (Sampling a, MonadState (Method a) m, MonadIO m) => m ()
run = do
  date_start <- timestamp

  looper 0

  date_end <- timestamp
  $notDone

looper :: (Sampling a, MonadState (Method a) m, MonadIO m) =>  Int -> m ()
looper i = do
  (Method MCommon{mIterations} _) <- get
  if i == mIterations then
      return ()
  else
    doIteration >>= \case
      SamplingSuccess -> looper (i + 1)
      _               -> looper (i)



randomPoint :: (Sampling a, MonadState (Method a) m, MonadIO m) =>  m Point
randomPoint = $notDone

createRunParamAndStoreQuality :: (Sampling a, MonadState (Method a) m, MonadIO m) => Point -> m ()
createRunParamAndStoreQuality point = $notDone

storeDataPoint :: (Sampling a, MonadState (Method a) m, MonadIO m) => Point -> m ()
storeDataPoint point = $notDone
