module TestGen.Helpers.IO where

import TestGen.Prelude
import Data.Time(formatTime,getCurrentTime)
import System.Locale(defaultTimeLocale)


timestamp :: IO Int
timestamp = do
    epochInt <- (readNote "timestamp" <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime) :: IO Int
    return epochInt
