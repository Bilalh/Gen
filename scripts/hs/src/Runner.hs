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

data ResultI = ResultI {
         all_finished :: Bool
        ,erroed :: Maybe Int
        ,given_time :: Float
        ,total_cpu_time :: Float
        ,total_real_time :: Float
        ,results :: [CmdI]
    } deriving(Show, Generic)

data CmdI = CmdI {
     rcode :: Int
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

runToolChain :: FilePath -> FilePath -> Int -> IO ResultI
runToolChain spec dir timeou = do
    let toolchain="/Users/bilalh/CS/instancegen/scripts/run/toolchain.py"
    code <- rawSystem toolchain [spec, "--outdir", dir , "--timeout", show timeou]
    print code
    result <- getJSONResult $ dir </> "result.json"
    return $ fromMaybe (error "no result") result
