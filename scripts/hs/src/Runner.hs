{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Runner where


import Data.Aeson(FromJSON(..),ToJSON(..))
import Data.Maybe(fromMaybe)
import Data.Map(Map)

import GHC.Generics (Generic)
import System.Environment(getEnv)
import System.FilePath.Posix((</>))
import System.Process(rawSystem)

import Data.Functor((<$>))

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B

-- For reading json from toolchain.py

data StatusI = Success_ | Timeout_  | ErrorUnknown_ | NumberToLarge_
    deriving (Show,Eq,Enum,Generic)

data ResultI = ResultI {
         erroed          :: Maybe Int
        ,given_time      :: Float
        ,last_status     :: StatusI
        ,result_dir      :: FilePath
        ,results         :: [CmdI]
        ,total_cpu_time  :: Float
        ,total_real_time :: Float
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

newtype RefineR = RefineR (Map String CmdI) deriving Show
instance FromJSON RefineR where
  parseJSON val = RefineR <$> parseJSON val

newtype SolveR = SolveR (Map String ResultI) deriving Show
instance FromJSON SolveR where
  parseJSON val = SolveR <$> parseJSON val


getJSONResult :: FilePath ->  IO (Maybe ResultI)
getJSONResult fp = fmap A.decode $ B.readFile fp

getJSONRefine :: FilePath ->  IO (Maybe RefineR)
getJSONRefine fp = fmap A.decode $ B.readFile fp

getJSONSolve :: FilePath ->  IO (Maybe SolveR)
getJSONSolve fp = fmap A.decode $ B.readFile fp

runToolChain :: FilePath -> FilePath -> Int -> IO ResultI
runToolChain spec dir timeou = do
    pg <- getEnv "PARAM_GEN_SCRIPTS"
    let toolchain= pg </> "run/toolchain.py"
    _ <- rawSystem toolchain [spec, "--outdir", dir , "--timeout", show timeou]
    result <- getJSONResult $ dir </> "result.json"
    return $ fromMaybe (error "no result") result
