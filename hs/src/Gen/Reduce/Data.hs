{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, KindSignatures, Rank2Types #-}
module Gen.Reduce.Data where

import Gen.Helpers.Log
import Gen.Imports
import Gen.IO.RunResult
import Gen.IO.Toolchain          (KindI, StatusI, ToolchainOutput (..))
import System.Random.TF
import Gen.Reduce.Random

import qualified Text.PrettyPrint    as Pr

type RRR a = forall m
  . (MonadIO m, MonadState RState m, RndGen m, MonadR m, MonadDB m, MonadLog m)
  => m a

data RConfig = RConfig
    { oErrKind_         :: KindI
    , oErrStatus_       :: StatusI
    , oErrChoices_      :: Maybe FilePath
    , outputDir_        :: FilePath
    , specDir_          :: FilePath
    , cores_            :: Int
    , specTime_         :: Int

    -- def Initialised
    , binariesDirectory_ :: Maybe FilePath
    , toolchainOutput_   :: ToolchainOutput

    , deletePassing_      :: Bool
    , resultsDB_dir       :: Maybe FilePath

    , totalIsRealTime_    :: Bool
    } deriving Show

data RState = RState
    { rconfig             :: RConfig
    , rgen_               :: TFGen
    , mostReduced_        :: Maybe ErrData
    , mostReducedChoices_ :: Maybe FilePath
    , otherErrors_        :: [ErrData]
    , timeLeft_           :: Maybe Int
    , resultsDB_          :: ResultsDB
    } deriving (Show)


instance Pretty RConfig where
    pretty = pretty . groom

instance Pretty RState where
    pretty RState{..} =
        "RState" <+> Pr.braces (
            Pr.sep
                [ nn "rconfig ="  rconfig
                , nn "mostReduced_ =" mostReduced_
                , nn "mostReducedChoices_ =" mostReducedChoices_
                , nn "timeLeft_ = " timeLeft_
                , nn "otherErrors_ =" (prettyArr otherErrors_)
                , nn "rgen_ =" (show rgen_)
                ])

instance Default RConfig where
    def = RConfig
          {oErrKind_           = error "need oErrKind_"
          ,oErrStatus_         = error "need oErrStatus_"
          ,oErrChoices_        = error "need oErrChoices_"
          ,cores_              = error "need cores"
          ,outputDir_          = error "need outputDir_"
          ,specDir_            = error "need specDir_"
          ,specTime_           = error "need specTime_"
          ,binariesDirectory_  = Nothing
          ,toolchainOutput_    = def
          ,deletePassing_      = False
          ,resultsDB_dir       = Nothing
          ,totalIsRealTime_    = False
          }

instance Default RState where
    def =  RState{rconfig             = def
                 ,resultsDB_          = def
                 ,rgen_               = error "need rgen_"
                 ,mostReduced_        = Nothing
                 ,otherErrors_        = []
                 ,mostReducedChoices_ = error "set mostReducedChoices_=oErrChoices_"
                 ,timeLeft_           = Nothing
                 }


mkrGen :: Int -> TFGen
mkrGen = mkTFGen


data EState = EState
  { spec_  :: Spec
  , sgen_  :: TFGen
  }

type ES a = StateT EState Identity a


newEState :: RndGen m => Spec -> m EState
newEState sp = do
  newSeed <- chooseR (0 :: Int ,2^(24:: Int) )
  return $ EState{spec_=sp,sgen_=mkrGen newSeed}

newEStateWithSeed :: Int -> Spec -> EState
newEStateWithSeed seed sp = do
  EState{spec_=sp,sgen_=mkrGen seed}



data WithGen a = WithGen { withGen_gen :: TFGen
                         , withGen_val :: a
                         }


withGen_new :: RndGen m => a -> m (WithGen a)
withGen_new a = do
  g <- getGen
  return $ WithGen{withGen_gen=g, withGen_val = a}

withGen_put :: forall (m :: * -> *) t.
               MonadState (WithGen t) m =>
               t -> m ()
withGen_put val = do
  modify $ \st -> st{withGen_val = val }


instance Monad m => MonadDB (StateT RState  m) where
  getsDb             = gets  resultsDB_
  putsDb db          = modify $ \st -> st{resultsDB_=db}
  getDbDirectory     = gets rconfig >>= return . resultsDB_dir
  getOutputDirectory = gets rconfig >>= return . outputDir_


class Monad m => MonadR m where
  getRconfig        :: m RConfig
  getChoicesToUse   :: m (Maybe FilePath)
  processOtherError :: ErrData -> m ()
  processPassing    :: Spec -> m ()

instance Monad m => MonadR (StateT RState m) where
  getRconfig          = gets rconfig
  getChoicesToUse     = gets mostReducedChoices_
  processOtherError r = modify $ \st -> st{otherErrors_ =r : otherErrors_ st }
  processPassing    _ = return ()
