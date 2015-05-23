{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Log where
import Gen.Imports

-- import System.IO.Unsafe(unsafePerformIO)
-- import System.IO(appendFile)
-- instance (Monad a, Applicative a)
--       => MonadLog (WriterT [(LogLevel, Doc)] a)  where
--     log lvl msg = do
--       unsafePerformIO $ do
--          appendFile "_unsafe.log" (show msg)
--          return $ tell []
--       tell [(lvl, msg)]

instance (Monad a, Applicative a)
      => MonadLog (WriterT [(LogLevel, Doc)] a)  where
    log lvl msg =
#ifdef GEN_TRACE
      when (lvl <= LogInfo) $  trace (" ^" ++ show lvl ++ " " ++ show msg)
#endif
      tell [(lvl, msg)]
