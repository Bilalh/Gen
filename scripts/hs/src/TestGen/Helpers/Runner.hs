{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module TestGen.Helpers.Runner where



import Data.Aeson(FromJSON(..),ToJSON(..))
import Data.Functor((<$>))
import Data.Map(Map)

import GHC.Generics (Generic)
import System.Directory(doesFileExist)
import System.Environment(getEnv)
import System.FilePath((</>))
import System.Process(rawSystem)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B

-- For reading json from toolchain.py

data StatusI =
      Success_
    | Timeout_
    | ErrorUnknown_
    | NumberToLarge_
    | HeapSpace_
    deriving (Show,Eq,Enum,Generic)

data ResultI = ResultI {
         erroed          :: Maybe Int
        ,last_status     :: StatusI
        ,results         :: [CmdI]
        ,total_cpu_time  :: Float
        ,total_real_time :: Float
    } deriving(Show, Generic)

data CmdI = CmdI {
     rcode     :: Int
    ,cpu_time  :: Float
    ,real_time :: Float
    ,finished  :: Bool
    ,timeout   :: Float
    ,status_   :: StatusI
    ,cmd       :: [String]

} deriving(Show, Generic)

type SolveR  = SettingI SolveM
type RefineR = SettingI RefineM
data SettingI a = SettingI {
         essence_    :: String
        ,given_time_ :: Float
        ,outdir_     :: FilePath
        ,successful_ :: Bool
        ,time_taken_ :: Float
        ,data_       :: a
        ,consistent_ :: Bool
    } deriving (Show, Generic)

instance (FromJSON a) => FromJSON (SettingI a)
instance (ToJSON a)   => ToJSON (SettingI a)

instance FromJSON ResultI
instance ToJSON   ResultI

instance FromJSON CmdI
instance ToJSON   CmdI

instance FromJSON StatusI
instance ToJSON   StatusI

newtype RefineM = RefineM (Map String CmdI) deriving Show
instance FromJSON RefineM where
  parseJSON val = RefineM <$> parseJSON val
instance ToJSON RefineM where
    toJSON (RefineM m) =  toJSON m

newtype SolveM = SolveM (Map String ResultI) deriving Show
instance FromJSON SolveM where
  parseJSON val = SolveM <$> parseJSON val
instance ToJSON SolveM where
    toJSON (SolveM m) = toJSON m


getJSON :: FromJSON a => FilePath -> IO (Maybe a)
getJSON fp = do
    b <- doesFileExist fp
    if b then
        fmap A.decode $ B.readFile fp
    else
        return Nothing

runToolChain :: FilePath -> FilePath -> Int -> IO (Either RefineR (RefineR, SolveR ) )
runToolChain = runToolChain1 4

runToolChain1 :: Int -> FilePath -> FilePath -> Int -> IO (Either RefineR (RefineR, SolveR ) )
runToolChain1 cores spec dir timeou = do
    pg <- getEnv "PARAM_GEN_SCRIPTS"
    let toolchain= pg </> "toolchain" </> "toolchain.py"
        args = [spec, "--outdir", dir 
               ,"--timeout", show timeou, "--num_cores", (show cores)]
    putStrLn $ "cmd: " ++ toolchain ++ " " ++ foldl1 (\a b -> a ++ " " ++ b) args
    _       <- rawSystem toolchain args
    refineF <- getJSON $ dir </> "refine_essence.json"
    solveF  <- getJSON $ dir </> "solve_eprime.json"

    return $ case (refineF, solveF) of
        (Just r, Just s)  -> Right (r,s)
        (Just r, Nothing) -> Left r
        (r, s)            -> error . show $ (r,s)

runRefine :: Int -> FilePath -> FilePath -> Int -> IO RefineR
runRefine cores spec dir timeou = do
    pg <- getEnv "PARAM_GEN_SCRIPTS"
    let toolchain= pg </> "toolchain" </> "toolchain.py"
        args = [spec, "--refine_only", "--outdir", dir
               , "--timeout", show timeou, "--num_cores", (show cores)]
    putStrLn $ "cmd: " ++ toolchain ++ " " ++ foldl1 (\a b -> a ++ " " ++ b) args
    _       <- rawSystem toolchain args
    refineF <- getJSON $ dir </> "refine_essence.json"

    return $ case (refineF) of
        Just r  ->  r
        Nothing -> error $ "script error" ++  "cmd: " ++ toolchain ++ " " ++ foldl1 (\a b -> a ++ " " ++ b) args
