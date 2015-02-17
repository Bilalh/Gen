{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module TestGen.Helpers.Runner where

import TestGen.Prelude
import Conjure.Language.Definition(Model)
import Conjure.UI.IO(writeModel)

import Data.Map(Map)

import System.Environment(getEnv)
import System.FilePath((<.>))
import System.Process(rawSystem)

import Data.List(foldl1)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B

import Data.Data

type Cores = Int
type Seed  = Int

-- For reading json from toolchain.py

data StatusI =
      Success_
    | Timeout_
    | ErrorUnknown_
    | NumberToLarge_
    | HeapSpace_
    | CannotEvaluate_
    | ValueNotInDom_
    | ParseError_
    | TypeChecking_
    | VarDuplicated_
    | NegativeExponent_
    | DivideByZero_
    | ConjureNA_
    | ConjureInvalid_
    | JavaException_
    | NotAHomoType_
    | ForgetRepr_
    | NotRefined_
    | UnknownLexeme_
    | StatusAny_
    deriving (Show,Eq,Enum,Generic,Typeable, Data, Read)

instance Pretty StatusI where
    pretty = pretty . show

data KindI =
      RefineCompact_
    | RefineAll_
    | RefineRandom_
    | RefineParam_
    | Savilerow_
    | TranslateUp_
    | Validate_
    | ValidateOld_
    | KindAny_
    deriving (Show,Eq,Enum,Generic,Typeable, Data, Read)


instance Pretty KindI where
    pretty = pretty . show


data ResultI = ResultI {
         erroed          :: Maybe Int
        ,last_status     :: StatusI
        ,last_kind       :: KindI
        ,results         :: [CmdI]
        ,total_cpu_time  :: Double
        ,total_real_time :: Double
    } deriving(Show, Generic)



data CmdI = CmdI {
     rcode     :: Int
    ,cpu_time  :: Double
    ,real_time :: Double
    ,finished  :: Bool
    ,timeout   :: Double
    ,status_   :: StatusI
    ,kind_     :: KindI
    ,cmd       :: [String]
    ,vals      :: ValsM

} deriving(Show, Generic)

type SolveR  = SettingI SolveM
type RefineR = SettingI RefineM
data SettingI a = SettingI {
         essence_    :: String
        ,given_time_ :: Double
        ,outdir_     :: FilePath
        ,successful_ :: Bool
        ,time_taken_ :: Double
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

instance FromJSON KindI
instance ToJSON   KindI

newtype ValsM = ValsM (Map String A.Value) deriving Show
instance FromJSON ValsM where
  parseJSON val = ValsM <$> parseJSON val
instance ToJSON ValsM where
    toJSON (ValsM m) =  toJSON m


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

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON fp a = do
  let s = A.encode a
  B.writeFile fp s


runToolChain :: FilePath -> FilePath -> Int -> IO (Either RefineR (RefineR, SolveR ) )
runToolChain spec dir timeou  = do
    seed <- randomRIO (0,2^(24 :: Int)) :: IO Int
    runToolChain1 seed False False 4 spec dir timeou

runToolChain1 :: Int -> Bool -> Bool -> Int -> FilePath -> FilePath -> Int
    -> IO (Either RefineR (RefineR, SolveR ) )
runToolChain1 seed newConjure refineAll cores spec dir timeou  = do
    let nc = if newConjure then ["--new_conjure"] else []
    let ra = if refineAll then ["--refine_all"] else []

    pg <- getEnv "PARAM_GEN_SCRIPTS"
    let toolchain= pg </> "toolchain" </> "toolchain_wrap.sh"
        args = [spec, "--outdir", dir
               ,"--timeout", show timeou
               , "--num_cores", (show cores), "--seed", (show seed)] ++ nc ++ ra
    putStrLn $ "cmd: " ++ toolchain ++ " " ++ foldl1 (\a b -> a ++ " " ++ b) args
    _       <- rawSystem toolchain args
    refineF <- getJSON $ dir </> "refine_essence.json"
    solveF  <- getJSON $ dir </> "solve_eprime.json"

    return $ case (refineF, solveF) of
        (Just r, Just s)  -> Right (r,s)
        (Just r, Nothing) -> Left r
        (r, s)            -> error . show $ (r,s)

runRefine :: Int -> FilePath -> FilePath -> Int -> IO RefineR
runRefine  cores spec dir timeou = do
    seed <- randomRIO (0,2^(24:: Int)) :: IO Int
    runRefine1 seed False cores spec dir timeou

runRefine1 :: Int -> Bool -> Int -> FilePath -> FilePath -> Int -> IO RefineR
runRefine1 seed newConjure cores spec dir timeou = do
    let nc = if newConjure then ["--new_conjure"] else []

    pg <- getEnv "PARAM_GEN_SCRIPTS"
    let toolchain= pg </> "toolchain" </> "toolchain_wrap.sh"
        args = [spec, "--refine_only", "--outdir", dir
               , "--timeout", show timeou
               , "--num_cores", (show cores), "--seed", (show seed)] ++ nc
    putStrLn $ "cmd: " ++ toolchain ++ " " ++ foldl1 (\a b -> a ++ " " ++ b) args
    _       <- rawSystem toolchain args
    refineF <- getJSON $ dir </> "refine_essence.json"

    return $ case (refineF) of
        Just r  ->  r
        Nothing -> error $ "script error" ++  "cmd: " ++ toolchain ++ " " ++ foldl1 (\a b -> a ++ " " ++ b) args

runRefine' :: Seed -> Cores -> Model -> FilePath -> Int -> Bool -> IO RefineR
runRefine' seed cores spec dir specTime newConjure = do
    print . pretty $ spec

    createDirectoryIfMissing True  dir

    let name = (dir </> "spec" <.> ".essence")
    writeModel (Just name) spec

    let specLim = specTime
    result <- runRefine1 seed newConjure cores name dir specLim
    putStrLn . groom $  result
    return result


runToolchain' :: Seed -> Int -> Model -> FilePath -> Int -> Bool -> Bool -> IO  (Either RefineR (RefineR, SolveR))
runToolchain' seed cores spec dir specTime newConjure refineAll= do
    putStrLn . renderSmall . nest 4 . vcat $ ["Running", pretty spec]

    createDirectoryIfMissing True  dir

    let name = (dir </> "spec" <.> ".essence")
    writeModel (Just name) spec

    let specLim = specTime
    result <- runToolChain1 seed newConjure refineAll cores name dir specLim
    -- putStrLn . groom $  result
    return result

kindsList :: [String]
kindsList = do
  let names = dataTypeConstrs . dataTypeOf $ (error "kindI" :: KindI)
  map show names

statusesList :: [String]
statusesList = do
  let names = dataTypeConstrs . dataTypeOf $ (error "StatusI" :: StatusI)
  map show names