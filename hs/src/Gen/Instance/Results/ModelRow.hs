{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
module Gen.Instance.Results.ModelRow where

import Gen.Imports
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField         ()
import Database.SQLite.Simple.FromRow           ()

data ModelRow = ModelRow
  { paramId              :: !(Int)
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
  , paramQuality         :: !(Double)
  , paramHash            :: !(String)
  } deriving (Generic, Show, Eq)

instance FromRow ModelRow where
    fromRow = ModelRow  <$> field <*> field <*> field <*> field
                        <*> field <*> field <*> field <*> field
                        <*> field <*> field <*> field <*> field
                        <*> field <*> field <*> field <*> field
