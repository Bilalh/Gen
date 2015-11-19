{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
module Gen.Instance.ModelInfo where

import Gen.Imports
import Data.Csv hiding (Only)

data ModelInfo = ModelInfo
  { essenceClass         :: !String
  , kind                 :: !String
  , heuristic            :: !(Maybe String)
  , run_no               :: !Int
  , paramId              :: !(Int)
  , eprimeId             :: !(Int)
  , eprime               :: !(String)
  , savileRow            :: !(Maybe Double)
  , minion               :: !(Maybe Double)
  , totalTime            :: !(Maybe Double)
  , minionNodes          :: !(Maybe Int)
  , minionTimeout        :: !(Maybe Int)
  , minionSolutionsFound :: !(Maybe Int)
  , minionSatisfiable    :: !(Int)
  , isOptimum            :: !(Int)
  , isDominated          :: !(Int)
  , solutionValue        :: !(Int)
  , minimising           :: !(Maybe Int)
  , isWinner             :: !(Int)
  , isCompact            :: !(Int)
  , isNoChan             :: !(Int)
  , fracId               :: !(Maybe Int)
  , numFractures         :: !Int
  , fracturesSize        :: !String
  , compactWon           :: !Int
  , paramQuality         :: !(Double)
  , mode                 :: !String
  , group                :: !Int
  , seq                  :: !Int
  , essence_name         :: !String
  , iterations           :: !Int
  , per_model_time_given :: !Int
  , use_all_solutions    :: !(Maybe String)
  , influence_radius     :: !(Maybe Int)
  , num_models           :: !Int
  , race_time_given      :: !Int
  , paramHash            :: !(String)
  , essence              :: !String
  , output_dir           :: !String
  } deriving (Generic, Show, Eq)

instance FromNamedRecord ModelInfo
instance ToNamedRecord  ModelInfo
instance DefaultOrdered ModelInfo
