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

data GenType = FirstGen
             | SecondGen
  deriving (Show, Data, Typeable, Eq)

instance Default GenType where
    def = FirstGen

data EssenceConfig = EssenceConfig
      { outputDirectory_ :: Directory
      , mode_            :: EssenceMode
      , genType_         :: GenType

      , totalTime_       :: Int
      , perSpecTime_     :: Int
      , size_            :: Int  -- generally less then 5
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
      } deriving (Show)

instance Default EssenceConfig where
    def = EssenceConfig
      { outputDirectory_ = error "EssenceConfig outputDirectory not set"
      , mode_            = Solve_

      , totalTime_   = error "EssenceConfig totalTime not set"
      , perSpecTime_ = error "EssenceConfig perSpecTime not set"
      , size_        = 4
      , cores_       = error "EssenceConfig cores_ not set"
      , seed_        = error "EssenceConfig seed_ not set"

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
      }
