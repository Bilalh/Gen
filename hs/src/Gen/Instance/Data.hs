module Gen.Instance.Data where

import Gen.Imports
import Conjure.Language.Definition

-- import qualified Text.PrettyPrint   as Pr

data Method kind = Method MCommon kind

data MCommon = MCommon
  { mEssenceFile :: FilePath
  , mOutputDir   :: FilePath        -- Where to put the results.
  , mRaceTimeout :: FilePath        -- Time per race
  , mVarInfo     :: FilePath        -- Variable Ordering
  , mPreGenerate :: Maybe FilePath  -- Generate all solution once and pick from them
  , mIterations  :: Int
  }

data Uniform = Uniform

data Markov = Markov
  { extra2 :: Int
  }

data SamplingResult = SamplingSuccess
                    | SamplingNoValuesLeft
                    | SamplingFailedToGenerateParam
                    | SamplingDontCountIteration

class Sampling a where
    doIteration :: (MonadIO m, MonadState (Method a) m) => m SamplingResult
