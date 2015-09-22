{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             DeriveTraversable #-}
module Gen.Instance.Data where

import Gen.Imports

import qualified Data.Aeson as A
import qualified Data.Set   as S
-- import qualified Text.PrettyPrint   as Pr


data Method kind = Method MCommon kind
  deriving (Eq, Show, Data, Typeable, Generic)

data MCommon = MCommon
  { mEssencePath  :: FilePath
  , mOutputDir    :: FilePath        -- | Where to put the results.
  , mModelTimeout :: Int             -- | Total time for each model in the race
  , mVarInfo      :: VarInfo         -- | Variable Ordering
  , mPreGenerate  :: Maybe FilePath  -- | Generate all solution once and pick from them
  , mIterations   :: Int             -- | Number of races to run
  } deriving (Eq, Show, Data, Typeable, Generic)

data Uniform = Uniform
  deriving (Eq, Show, Data, Typeable, Generic)

data Markov = Markov
  { extra2 :: Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

data SamplingResult = SamplingSuccess
                    | SamplingNoValuesLeft
                    | SamplingFailedToGenerateParam
                    | SamplingDontCountIteration
  deriving (Eq, Show, Data, Typeable, Generic)

class Sampling a where
    doIteration :: (MonadIO m, MonadState (Method a) m) => m SamplingResult

type Point = [(Text,Int)]

-- info.json
data VarInfo =
  VarInfo { ordering :: [Text]     -- | Order to generate the variables
          , givens   :: S.Set Text -- | givens which are not converted
          , finds    :: S.Set Text -- | givens to be converted to finds
          } deriving (Eq, Show, Data, Typeable, Generic)

instance A.FromJSON VarInfo
instance A.ToJSON VarInfo
