{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
module Gen.Instance.Results.Versions where

import Data.Csv         hiding (Only)
import Data.Time        (formatTime)
import Data.Time.Clock  (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Gen.Imports
import Data.Char(toUpper)

import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import qualified Control.Monad

newtype ISODate = ISODate { unIsoDate :: UTCTime } deriving (Show, Eq, Ord)

instance ToField ISODate where
  toField (ISODate tm) =
      fromString $ (formatTime defaultTimeLocale "%F_%H-%M_%s" tm)

instance FromField ISODate where
  parseField =  fmap (ISODate <$> parseTimeOrError True defaultTimeLocale "%s"   )
             .  fmap (T.unpack . last . T.split (=='_') ) . parseField
      -- fromString $ (formatTime defaultTimeLocale "%F_%H-%M_%s" tm)

data SCM = GIT | HG | SVN
  deriving (Generic, Show, Eq, Read)

instance FromField SCM where
    parseField s = case readMay (map toUpper (B.unpack s)) of
                     Just x  -> return x
                     Nothing -> Control.Monad.fail $  "Failed to parse " ++ B.unpack s


instance ToField SCM where
    toField = B.pack  .  map toLower . show


data Version = Version
  { name      :: String
  , scm       :: SCM
  , hash      :: String
  , ver_date  :: ISODate
  , uname     :: String
  , whoami    :: String
  , host_type :: String
  , hostname  :: String
  } deriving (Generic, Show, Eq)

instance FromNamedRecord Version
instance ToNamedRecord Version
instance DefaultOrdered Version
