{-# LANGUAGE DeriveDataTypeable #-}

module Gen.Essence.UIData  where

import Gen.Imports
import Gen.IO.Toolchain(KindI,StatusI,ToolchainOutput)

data EssenceMode =
          TypeCheck_
        | Refine_
        | Solve_
  deriving (Show, Data, Typeable, Eq)

instance Default EssenceMode where
    def = Solve_

data GenType = SecondGen
  deriving (Show, Data, Typeable, Eq)

instance Default GenType where
    def = SecondGen

data EssenceConfig = EssenceConfig
      { outputDirectory_ :: Directory
      , mode_            :: EssenceMode
      , genType_         :: GenType

      , totalTime_       :: Int
      , perSpecTime_     :: Int
      , domainDepth_     :: Int
      , expressionDepth_ :: Int
      , cores_           :: Int
      , seed_            :: Int

      , totalIsRealTime    :: Bool
      , deletePassing_     :: Bool
      , binariesDirectory_ :: Maybe Directory
      , oldConjure_        :: Bool
      , toolchainOutput_   :: ToolchainOutput
      , notUseful          :: Set ( KindI, StatusI )
      , givenSpecs_        :: Maybe [FilePath]
      , dbDirectory_       :: Maybe Directory
      , reduceAsWell_      :: Maybe Time
      , logLevel           :: LogLevel
      , strictTypeChecking :: Bool
      } deriving (Show)

instance Default EssenceConfig where
    def = EssenceConfig
      { outputDirectory_   = error "EssenceConfig outputDirectory not set"
      , totalTime_         = error "EssenceConfig totalTime not set"
      , perSpecTime_       = error "EssenceConfig perSpecTime not set"
      , cores_             = error "EssenceConfig cores_ not set"
      , seed_              = error "EssenceConfig seed_ not set"
      , domainDepth_       = error "EssenceConfig domainDepth_ not set"
      , expressionDepth_   = error "EssenceConfig expressionDepth_ not set"

      , mode_ = Solve_
      , totalIsRealTime    = True
      , deletePassing_     = False
      , binariesDirectory_ = Nothing
      , oldConjure_        = False
      , toolchainOutput_   = def
      , notUseful          = def
      , givenSpecs_        = Nothing
      , genType_           = def
      , reduceAsWell_      = Nothing
      , dbDirectory_       = Nothing
      , logLevel           = LogDebug
      , strictTypeChecking = False
      }


data Instance_Common = Instance_Common
    { essence_path       :: FilePath
    , per_model_time     :: Int
    , iterations         :: Int
    , mode               :: String
    , output_directory   :: Maybe FilePath
    , log_level          :: LogLevel
    } deriving (Show, Data, Typeable, Eq)
