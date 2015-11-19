{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
module Gen.Instance.Results.SettingsIn where

import Gen.Imports
import Data.Csv hiding (Only)

data CSV_IN = CSV_IN
  { seq                  :: !Int
  , run_no               :: !Int
  , kind                 :: !String
  , essence_name         :: !String
  , mode                 :: !String
  , iterations           :: !Int
  , per_model_time_given :: !Int
  , use_all_solutions    :: !(Maybe String)
  , influence_radius     :: !(Maybe Int)
  , num_models           :: !Int
  , race_time_given      :: !Int
  , group                :: !Int
  , essence              :: !String
  , output_dir           :: !String
  } deriving (Generic, Show, Eq)

instance FromNamedRecord CSV_IN
instance ToNamedRecord CSV_IN
instance DefaultOrdered CSV_IN
