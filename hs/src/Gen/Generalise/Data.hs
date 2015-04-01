{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, FlexibleInstances,
             KindSignatures #-}
module Gen.Generalise.Data where

import Gen.IO.Toolchain (KindI, StatusI)
import Gen.Prelude
import Gen.Reduce.Data  hiding (RState)
import System.Random.TF

import qualified Data.HashMap.Strict as H
import qualified Text.PrettyPrint    as Pr


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

    , mostReduced_        :: Maybe RunResult
    , mostReducedChoices_ :: Maybe FilePath
    , otherErrors_        :: [RunResult]
    , rlogs_              :: LogsTree
    , deletePassing_      :: Bool

    , resultsDB_          :: ResultsDB
    , resultsDB_dir       :: Maybe FilePath
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

                , nn "mostReduced_ =" mostReduced_
                , nn "mostReducedChoices_ =" mostReducedChoices_
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
                 ,mostReduced_        = Nothing
                 ,otherErrors_        = []
                 ,rlogs_              = LSEmpty
                 ,binariesDirectory_  = Nothing
                 ,toolchainOutput_    = def
                 ,deletePassing_      = False
                 ,resultsDB_          = H.empty
                 ,mostReducedChoices_ = error "set mostReducedChoices_=oErrChoices_"
                 ,resultsDB_dir       = Nothing
                 }
