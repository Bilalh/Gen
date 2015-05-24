{-# LANGUAGE DeriveDataTypeable #-}

module Gen.Essence.UIData  where

import Gen.Imports
import Gen.IO.Toolchain(KindI,StatusI,ToolchainOutput)
import Data.IntSet

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
      { outputDirectory_ :: FilePath
      , mode_            :: EssenceMode
      , genType_         :: GenType

      , totalTime_       :: Int
      , perSpecTime_     :: Int
      , size_            :: Int  -- should not be greater then 5
      , cores_           :: Int
      , seed_            :: Int

      , totalIsRealTime    :: Bool
      , deletePassing_     :: Bool
      , binariesDirectory_ :: Maybe FilePath
      , oldConjure_        :: Bool
      , toolchainOutput_   :: ToolchainOutput
      , notUseful          :: Set ( KindI, StatusI )
      , givenSpecs_        :: Maybe [FilePath]
      , runHashes_         :: IntSet

      , reduceAsWell_      :: Maybe Int
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
      , runHashes_         = def
      , genType_           = def
      , reduceAsWell_      = Nothing
      }
