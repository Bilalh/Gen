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
    { oErrKind_         :: KindI
    , oErrStatus_       :: StatusI
    , oErrChoices_      :: Maybe FilePath
    , outputDir_        :: FilePath
    , specDir_          :: FilePath

    , cores_            :: Int
    , rgen_             :: TFGen
    , specTime_         :: Int

    -- def Initialised
    , binariesDirectory_ :: Maybe FilePath
    , toolchainOutput_   :: ToolchainOutput

    , choicesToUse_      :: Maybe FilePath
    , rlogs_             :: LogsTree
    , deletePassing_     :: Bool
    , otherErrors_       :: [ErrData]

    , resultsDB_         :: ResultsDB
    , resultsDB_dir      :: Maybe FilePath
    } deriving (Show)


instance Pretty GState where
    pretty GState{..} =
        "GState" <+> Pr.braces (
            Pr.sep
                [ nn "oErrKind_ = "  oErrKind_
                , nn "oErrStatus_ =" oErrStatus_
                , nn "oErrChoices_ =" oErrChoices_

                , nn "outputDir_" outputDir_
                , nn "specDir_" specDir_

                , nn "cores_" cores_
                , nn "specTime_" specTime_
                , nn "binariesDirectory_" binariesDirectory_
                , nn "toolchainOutput_" toolchainOutput_

                , nn "choicesToUse =" choicesToUse_
                , nn "otherErrors_ =" (prettyArr otherErrors_)

                -- , nn "rgen_ =" (show rgen_)
                ])

instance Default GState where
    def =  GState{oErrKind_           = error "need oErrKind_"
                 ,oErrStatus_         = error "need oErrStatus_"
                 ,oErrChoices_        = error "need oErrChoices_"
                 ,cores_              = error "need cores"
                 ,outputDir_          = error "need outputDir_"
                 ,rgen_               = error "need rgen_"
                 ,specDir_            = error "need specDir_"
                 ,specTime_           = error "need specTime_"
                 ,otherErrors_        = []
                 ,rlogs_              = LSEmpty
                 ,binariesDirectory_  = Nothing
                 ,toolchainOutput_    = def
                 ,deletePassing_      = False
                 ,resultsDB_          = def
                 ,choicesToUse_       = error "set mostReducedChoices_=oErrChoices_"
                 ,resultsDB_dir       = Nothing
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
    getDbDirectory     = gets resultsDB_dir
    getOutputDirectory = gets outputDir_
