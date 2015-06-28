{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Log where
import Gen.Imports

newtype LogT m a = LogT (WriterT [(LogLevel, Doc)] m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (Applicative m, Monad m) => MonadLog (LogT m) where
  log lvl msg =
#ifdef GEN_TRACE
    when (lvl <= LogInfo) $  trace (" ^" ++ show lvl ++ " " ++ show msg)
#endif
    LogT $ tell [(lvl, msg)]

instance MonadFail m => MonadFail (LogT m) where
    fail = lift . fail

runLogT :: Monad m => LogLevel -> LogT m a -> m (a, [Doc])
runLogT l (LogT ma) = do
  (a, logs) <- runWriterT ma
  return (a, [ msg | (lvl, msg) <- logs , lvl <= l ])

runLogT2 :: Monad m => LogT m a -> m (a, [(LogLevel, Doc)])
runLogT2 (LogT ma) = do
  (a, logs) <- runWriterT ma
  return (a,logs)
