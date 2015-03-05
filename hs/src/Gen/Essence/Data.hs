{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Gen.Essence.Data  where

import Gen.Prelude

type Depth = Int
type Ref = Text

data EssenceMode =
          TypeCheck_
        | Refine_
        | Solve_
  deriving (Show, Data, Typeable, Eq)

instance Default EssenceMode where
    def = Solve_


data EssenceConfig = EssenceConfig
      { outputDirectory_ :: FilePath
      , mode_            :: EssenceMode

      , totalTime_       :: Int
      , perSpecTime_     :: Int
      , size_            :: Int  -- should not be greater then 5
      , cores_           :: Int
      , seed_            :: Int

      , totalIsRealTime    :: Bool
      , deletePassing_     :: Bool
      , binariesDirectory_ :: Maybe FilePath
      , oldConjure_        :: Bool
      }

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
      }
