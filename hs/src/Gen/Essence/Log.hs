{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Log where
import Gen.Imports

instance (Monad a, Applicative a)
      => MonadLog (WriterT [(LogLevel, Doc)] a)  where
    log lvl msg =
#ifdef GEN_TRACE
      when (lvl <= LogInfo) $  trace (" ^" ++ show lvl ++ " " ++ show msg)
#endif
      tell [(lvl, msg)]

newtype LogT m a = LogT (WriterT [(LogLevel, Doc)] m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (Applicative m, Monad m) => MonadLog (LogT m) where
    log lvl msg = LogT $ tell [(lvl, msg)]
