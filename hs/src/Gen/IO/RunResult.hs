{-# LANGUAGE DeriveDataTypeable, DeriveGeneric#-}
module Gen.IO.RunResult where

import Gen.Imports
import Gen.IO.Toolchain          (KindI, StatusI)

data RunResult =
    OurError{
      resDirectory_  :: FilePath
    , resErrKind_    :: KindI
    , resErrStatus_  :: StatusI
    , resErrChoices_ :: FilePath
    , timeTaken_     :: Int
    }
    | StoredError{
      resDirectory_  :: FilePath
    , resErrKind_    :: KindI
    , resErrStatus_  :: StatusI
    , resErrChoices_ :: FilePath
    , timeTaken_     :: Int
    }
    | Passing {
      timeTaken_     :: Int
    } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
