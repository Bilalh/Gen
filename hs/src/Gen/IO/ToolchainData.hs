{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.IO.ToolchainData where

import qualified Data.Aeson as A
import Data.Data
import Data.Map (Map)
import Gen.Prelude

data ToolchainData = ToolchainData
    {
      essencePath        :: FilePath
    , outputDirectory    :: FilePath
    , totalTime          :: Int

    , essenceParam       :: Maybe FilePath
    , refineType         :: RefineType

    , cores              :: Int
    , seed               :: Maybe Int

    , binariesDirectory  :: Maybe FilePath
    , oldConjure         :: Bool
    , toolchainOutput    :: ToolchainOutput
    , choicesPath        :: Maybe FilePath
    }
  deriving (Show, Eq, Generic, Typeable, Data)

instance Default ToolchainData where
    def = ToolchainData
          {
            essencePath       = $never
          , outputDirectory   = $never
          , totalTime         = $never
          , essenceParam      = Nothing
          , refineType        = def
          , cores             = $never
          , seed              = Nothing
          , binariesDirectory = Nothing
          , oldConjure        = False
          , toolchainOutput   = def
          , choicesPath       = Nothing 
          }

data RefineType =
                  Refine_Only
                | Refine_All
                | Refine_Solve
                | Refine_Solve_All
  deriving (Show, Data, Typeable,Eq)

instance Default RefineType where
    def = Refine_Solve

data ToolchainResult =  RefineResult RefineR | SolveResult (RefineR, SolveR )
  deriving (Show, Eq, Generic, Typeable, Data)



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
        ,last_kind       :: Maybe KindI
        ,results         :: [CmdI]
        ,total_cpu_time  :: Double
        ,total_real_time :: Double
    } deriving (Show, Eq, Generic, Typeable, Data)



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

} deriving (Show, Eq, Generic, Typeable, Data)

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
    } deriving (Show, Eq, Generic, Typeable, Data, Read)

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

newtype ValsM = ValsM (Map String A.Value)
    deriving (Show, Eq, Generic, Typeable, Data)

instance FromJSON ValsM where
  parseJSON val = ValsM <$> parseJSON val
instance ToJSON ValsM where
    toJSON (ValsM m) =  toJSON m


newtype RefineM = RefineM (Map String CmdI)
    deriving (Show, Eq, Generic, Typeable, Data)

instance FromJSON RefineM where
  parseJSON val = RefineM <$> parseJSON val
instance ToJSON RefineM where
    toJSON (RefineM m) =  toJSON m

newtype SolveM = SolveM (Map String ResultI)
    deriving (Show, Eq, Generic, Typeable, Data)

instance FromJSON SolveM where
  parseJSON val = SolveM <$> parseJSON val
instance ToJSON SolveM where
    toJSON (SolveM m) = toJSON m
