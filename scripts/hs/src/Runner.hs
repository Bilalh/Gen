{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Runner where

import qualified Data.Aeson as A
import Data.Aeson(FromJSON(..),ToJSON(..))
import qualified Data.ByteString.Lazy as B

import GHC.Generics (Generic)

-- For reading json from toolchain.py

data ResultI = ResultI {
         all_finished :: Bool
        ,erroed :: Maybe Int
        ,given_time :: Float
        ,total_cpu_time :: Float
        ,total_real_time :: Float
        ,results :: [CmdI]
    } deriving(Show, Generic)

data CmdI = CmdI {
     code :: Int
    ,cpu_time :: Float
    ,real_time :: Float
    ,finished :: Bool
    ,timeout :: Float
    ,cmd :: [String]

} deriving(Show, Generic)

instance FromJSON ResultI
instance ToJSON ResultI

instance FromJSON CmdI
instance ToJSON CmdI

getJSONResult :: FilePath ->  IO (Maybe ResultI)
getJSONResult fp = fmap A.decode $ B.readFile fp

