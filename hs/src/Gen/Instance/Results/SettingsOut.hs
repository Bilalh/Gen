{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
module Gen.Instance.Results.SettingsOut where

import Gen.Imports
import Data.Csv hiding (Only)

data CSV_OUT = CSV_OUT
  { seq                            :: !Int
  , run_no                         :: !Int
  , kind                           :: !String
  , essence_name                   :: !String
  , mode                           :: !String
  , iterations                     :: !Int
  , per_model_time_given           :: !Int
  , use_all_solutions              :: !(Maybe String)
  , influence_radius               :: !(Maybe Int)
  , num_models                     :: !Int
  , race_time_given                :: !Int
  , group                          :: !Int
  , essence                        :: !String
  , output_dir                     :: !String
  , essenceClass                   :: !String
  , heuristic                      :: !(Maybe String)
  , numFractures                   :: !Int
  , fracturesSize                  :: !String
  , fractures                      :: !String
  , compact                        :: !String
  , compactWon                     :: !Int
  , highestOrderingNeeded          :: !Int
  , rTimestampStart                :: !Int
  , rTimestampEnd                  :: !Int
  , rRealTime                      :: !Int
  , rCPUTime                       :: !Double
  , rRacesCPUTime                  :: !Double
  , rParamGenCPUTime               :: !Double
  , rSubCPUTime                    :: !Double
  , rOurCPUTime                    :: !Double
  , rIterationsDone                :: !Int
  , rIterationsDoneIncludingFailed :: !Int
  , hostType                       :: !String
  } deriving (Generic, Show, Eq)

instance FromNamedRecord CSV_OUT
instance ToNamedRecord CSV_OUT
instance DefaultOrdered CSV_OUT
