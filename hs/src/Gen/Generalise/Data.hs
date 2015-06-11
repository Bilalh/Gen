{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, KindSignatures #-}
module Gen.Generalise.Data where

import Gen.Helpers.Log
import Gen.Imports
import Gen.IO.Toolchain (KindI, StatusI, ToolchainOutput (..))
import Gen.Reduce.Data  hiding (RState (..))
import Gen.IO.RunResult
import System.Random.TF

import qualified Text.PrettyPrint    as Pr


type EE a = StateT GState IO a

data GState = GState
    { rconfig            :: RConfig
    , rgen_              :: TFGen
    , choicesToUse_      :: Maybe FilePath
    , rlogs_             :: LogsTree
    , otherErrors_       :: [ErrData]
    , resultsDB_         :: ResultsDB
    } deriving (Show)


instance Pretty GState where
    pretty GState{..} =
        "GState" <+> Pr.braces (
            Pr.sep
                [ nn "rconfig "  rconfig
                , nn "choicesToUse_ " choicesToUse_
                , nn "otherErrors_ " (prettyArr otherErrors_)
                , nn "rgen_ =" (show rgen_)
                ])

instance Default GState where
    def =  GState{rconfig       = def
                 ,otherErrors_  = []
                 ,rlogs_        = LSEmpty
                 ,resultsDB_    = def
                 ,choicesToUse_ = error "set mostReducedChoices_=oErrChoices_"
                 ,rgen_         = error "need rgen_"
                 }


instance HasLogger (StateT GState IO)  where
    getLog = gets rlogs_
    putLog lg = modify $ \st -> st{ rlogs_=lg}

instance HasGen (StateT GState IO) where
  getGen = gets rgen_
  putGen g = modify $ \st -> st{rgen_=g }


instance (HasLogger (StateT EState (IdentityT (StateT GState IO)))) where
    getLog = gets elogs_
    putLog lg = modify $ \st -> st{ elogs_=lg}

instance (HasGen (StateT EState (IdentityT (StateT GState IO)))) where
  getGen   = gets sgen_
  putGen g = modify $ \st -> st{sgen_=g }


instance Monad m => MonadDB (StateT GState m) where
  getsDb             = gets resultsDB_
  putsDb db          = modify $ \st -> st{resultsDB_=db}
  getDbDirectory     = gets rconfig >>= return . resultsDB_dir
  getOutputDirectory = gets rconfig >>= return . outputDir_


instance(Monad m, MonadIO m) => MonadR (StateT GState m) where
  getRconfig          = gets rconfig
  processOtherError r = modify $ \st -> st{otherErrors_ =r : otherErrors_ st }
  getChoicesToUse     = gets choicesToUse_
  processPassing    _ = return ()
