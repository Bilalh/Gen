{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             DeriveTraversable #-}
module Gen.Instance.Data where

import Gen.Imports
import Conjure.Language.Definition
import Gen.Instance.SamplingError

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
  , mPreGenerate    :: Maybe FilePath   -- | Generate all solution once and pick from them
  , mIterations     :: Int              -- | Number of races to run
  , mMode           :: String           -- | the directory suffix
  , mModelsDir      :: FilePath         -- | The models directory e.g. prob006-GR_df
  , mGivensProvider :: Provider         -- | Generating the given
  , mCores          :: Int
  , mCompactName    :: Maybe EprimeName -- | Ordering with compact first
  -- above fields do not change
  , mPoints         :: [Point]          -- |  The instance that have been run
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

instance Pretty Point where
    pretty (Point xs) = "Point:" <+>  (vcat . map pretty $ xs)

-- | Provides values for givens
newtype Provider = Provider [(Name, Domain () Constant)]
    deriving (Eq, Show, Data, Typeable, Generic)

instance A.FromJSON Provider
instance A.ToJSON Provider

data RunMetadata = RunMetadata
    { rTimestampStart                :: Int
    , rTimestampEnd                  :: Int
    , rRealTime                      :: Int
    , rCPUTime                       :: Double
    , rSubCPUTime                    :: Double
    , rOurCPUTime                    :: Double
    , rIterationsDone                :: Int
    , rIterationsDoneIncludingFailed :: Int
    } deriving (Eq, Show, Data, Typeable, Generic)


instance A.FromJSON RunMetadata
instance A.ToJSON RunMetadata

voidRes :: Monad m => m (Either a t) -> m (Either a ())
voidRes x = fmap (const ()) <$> x
-- voidRes x = do
--   res <- x
--   case res of
--     Left x -> return $ Left x
--     Right x -> return $ Right ()
