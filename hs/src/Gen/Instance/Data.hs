{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             DeriveTraversable #-}
module Gen.Instance.Data where

import Gen.Imports
import Gen.Instance.SamplingError
import Gen.Instance.Point

import qualified Data.Aeson as A
import qualified Data.Set   as S

-- | The class defines the how sampling occurs
-- | See undirected for an exmaple
class Sampling a where
    doIteration :: (MonadIO m, MonadState (Method a) m, MonadLog m)
                => m (Either SamplingErr ())


data Method kind = Method MCommon kind
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

instance A.FromJSON a => A.FromJSON (Method a)
instance A.ToJSON a   => A.ToJSON   (Method a)

type EprimeName = String
data MCommon = MCommon
  { mEssencePath    :: FilePath
  , mOutputDir      :: FilePath         -- | Where to put the results.
  , mModelTimeout   :: Int              -- | Total time for each model in the race
  , mVarInfo        :: VarInfo          -- | Variable Ordering
  , mIterations     :: Int              -- | Number of races to run
  , mMode           :: String           -- | the directory suffix
  , mModelsDir      :: FilePath         -- | The models directory e.g. prob006-GR_df
  , mGivensProvider :: Provider         -- | Generating the given
  , mCores          :: Int              -- | Number of cores to use
  , mCompactName    :: Maybe EprimeName -- | Ordering with compact first
  , mPreGenerate    :: Maybe (FilePath,Solutions) -- | Generate all solution once and pick from them
  -- above fields do not change
  , mPoints         :: [Point]          -- | Instances that have been run, newest first
  , mSubCpu         :: Double           -- | Other sub-processes
  , mPointsGiven    :: Maybe [Point]    -- | Uses these point for generation
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
instance Pretty SamplingResult where pretty = pretty . show

-- info.json
data VarInfo =
  VarInfo { givens   :: S.Set Text -- | givens which are not converted
          } deriving (Eq, Show, Data, Typeable, Generic)

instance A.FromJSON VarInfo
instance A.ToJSON VarInfo




data RunMetadata = RunMetadata
    { rTimestampStart                :: Int
    , rTimestampEnd                  :: Int
    , rRealTime                      :: Int
    , rCPUTime                       :: Double
    , rRacesCPUTime                  :: Double
    , rParamGenCPUTime               :: Double
    , rSubCPUTime                    :: Double  -- | Any other subprocess
    , rOurCPUTime                    :: Double
    , rIterationsDone                :: Int
    , rIterationsDoneIncludingFailed :: Int
    } deriving (Eq, Show, Data, Typeable, Generic)


instance A.FromJSON RunMetadata
instance A.ToJSON RunMetadata

type TimeStamp = Int
type Quality   = Double

voidRes :: Monad m => m (Either a t) -> m (Either a ())
voidRes x = fmap (const ()) <$> x
-- voidRes x = do
--   res <- x
--   case res of
--     Left x -> return $ Left x
--     Right x -> return $ Right ()

-- For all Solutions
data Solutions  = Solutions Int [SolutionCount]
  deriving (Eq, Show, Data, Typeable, Generic)
data SolutionCount = SolCount Int String
  deriving (Eq, Show, Data, Typeable, Generic)

instance A.FromJSON Solutions
instance A.ToJSON Solutions

instance A.FromJSON SolutionCount
instance A.ToJSON SolutionCount
