{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Runner where


import System.Process(rawSystem)
import Data.Aeson(FromJSON(..),ToJSON(..))
import GHC.Generics (Generic)
import System.FilePath.Posix((</>))
import Data.Maybe(fromMaybe)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B

-- For reading json from toolchain.py

data StatusI = Success_ | Timeout_  | ErrorUnknown_ | NumberToLarge_
    deriving (Show,Eq,Enum,Generic)

data ResultI = ResultI {
         all_finished :: Bool
        ,erroed :: Maybe Int
        ,given_time :: Float
        ,total_cpu_time :: Float
        ,total_real_time :: Float
        ,results :: [CmdI]
        -- ,last_status :: StatusI
    } deriving(Show, Generic)

data CmdI = CmdI {
     rcode :: Int
    ,cpu_time :: Float
    ,real_time :: Float
    ,finished :: Bool
    ,timeout :: Float
    ,status_ :: StatusI
    ,cmd :: [String]

} deriving(Show, Generic)

instance FromJSON ResultI
instance ToJSON ResultI

instance FromJSON CmdI
instance ToJSON CmdI

instance FromJSON StatusI
instance ToJSON StatusI


getJSONResult :: FilePath ->  IO (Maybe ResultI)
getJSONResult fp = fmap A.decode $ B.readFile fp

runToolChain :: FilePath -> FilePath -> Int -> IO ResultI
runToolChain spec dir timeou = do
    let toolchain="/Users/bilalh/CS/instancegen/scripts/run/toolchain.py"
    _ <- rawSystem toolchain [spec, "--outdir", dir , "--timeout", show timeou]
    result <- getJSONResult $ dir </> "result.json"
    return $ fromMaybe (error "no result") result
