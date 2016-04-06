{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, KindSignatures, Rank2Types #-}
module Gen.Generalise.Data where

import Gen.Imports
import Gen.Reduce.Data  hiding (RState (..))
import Gen.IO.RunResult
import Gen.Reduce.Random
import Gen.Helpers.MonadNote
import Gen.Essence.Id
import Gen.Essence.Data.Key

import qualified Text.PrettyPrint    as Pr

type EEE a = forall m .
 (MonadIO m, ReduceSettings m, MonadState GState m, RndGen m, MonadR m, MonadDB m, MonadLog m, MonadNote m)
 => m a


data GState = GState
    { rconfig            :: RConfig
    , choicesToUse_      :: Maybe FilePath
    , otherErrors_       :: [ErrData]
    , resultsDB_         :: ResultsDB
    , passingTrees       :: [KTree Key]
    } deriving (Show)


instance Pretty GState where
    pretty GState{..} =
        "GState" <+> Pr.braces (
            Pr.sep
                [ nn "rconfig "  rconfig
                , nn "choicesToUse_ " choicesToUse_
                , nn "otherErrors_ " (prettyArr otherErrors_)
                ])

instance Default GState where
    def =  GState{rconfig       = def
                 ,otherErrors_  = []
                 ,resultsDB_    = def
                 ,passingTrees  = []
                 ,choicesToUse_ = error "set mostReducedChoices_=oErrChoices_"
                 }


instance Monad m => MonadDB (StateT GState m) where
  getsDb             = gets resultsDB_
  putsDb db          = modify $ \st -> st{resultsDB_=db}
  getDbDirectory     = gets rconfig >>= return . resultsDB_dir
  getOutputDirectory = gets rconfig >>= return . outputDir_


instance(Monad m, MonadIO m) => MonadR (StateT GState m) where
  getRconfig          = gets rconfig
  processOtherError r = modify $ \st -> st{otherErrors_ =r : otherErrors_ st }
  getChoicesToUse     = gets choicesToUse_
  processPassing sp   = do
    let tree = keyTree sp
    modify $ \st -> st{passingTrees = tree : passingTrees st }
