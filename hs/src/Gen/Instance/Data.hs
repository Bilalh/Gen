{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             DeriveTraversable #-}
module Gen.Instance.Data where

import Gen.Imports
import Conjure.Language.Definition

import qualified Data.Aeson as A
import qualified Data.Set   as S

-- | The class defines the how sampling occurs
-- | See undirected for an exmaple
class Sampling a where
    doIteration :: (MonadIO m, MonadState (Method a) m, MonadLog m)
                => m SamplingResult


data Method kind = Method MCommon kind
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

instance A.FromJSON a => A.FromJSON (Method a)
instance A.ToJSON a   => A.ToJSON   (Method a)

data MCommon = MCommon
  { mEssencePath    :: FilePath
  , mOutputDir      :: FilePath        -- | Where to put the results.
  , mModelTimeout   :: Int             -- | Total time for each model in the race
  , mVarInfo        :: VarInfo         -- | Variable Ordering
  , mPreGenerate    :: Maybe FilePath  -- | Generate all solution once and pick from them
  , mIterations     :: Int             -- | Number of races to run
  , mMode           :: String          -- | the directory suffix
  , mGivensProvider :: Provider        -- | Generating the given
  , mCores          :: Int
  -- above fields do not change
  , mPoints         :: [Point]         -- |  The instance that have been run
  } deriving (Eq, Show, Data, Typeable, Generic)


instance A.FromJSON MCommon
instance A.ToJSON MCommon



data SamplingResult = SamplingSuccess
                    | SamplingNoValuesLeft
                    | SamplingFailedToGenerateParam
                    | SamplingDontCountIteration
  deriving (Eq, Show, Data, Typeable, Generic)

instance A.FromJSON SamplingResult
instance A.ToJSON SamplingResult


-- info.json
data VarInfo =
  VarInfo { ordering :: [Text]     -- | Order to generate the variables
          , givens   :: S.Set Text -- | givens which are not converted
          , finds    :: S.Set Text -- | givens to be converted to finds
          } deriving (Eq, Show, Data, Typeable, Generic)

instance A.FromJSON VarInfo
instance A.ToJSON VarInfo


-- | Param Values
newtype Point  = Point [(Name,Constant)]
 deriving (Eq, Show, Data, Typeable, Generic)

instance Monoid Point where
  mempty                        = Point []
  mappend (Point xs) (Point ys) = Point $ xs ++ ys

instance A.FromJSON Point
instance A.ToJSON Point


-- | Provides values for givens
newtype Provider = Provider [(Name, Domain () Constant)]
    deriving (Eq, Show, Data, Typeable, Generic)

instance A.FromJSON Provider
instance A.ToJSON Provider

data RunMetadata = RunMetadata
    { rTimestampStart                :: Int
    , rTimestampEnd                  :: Int
    , rRealTime                      :: Int
    , rCPUTime                       :: Int
    , rIterationsDone                :: Int
     ,rIterationsDoneIncludingFailed :: Int
    } deriving (Eq, Show, Data, Typeable, Generic)


instance A.FromJSON RunMetadata
instance A.ToJSON RunMetadata
