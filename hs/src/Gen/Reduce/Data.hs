{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, KindSignatures, Rank2Types #-}
module Gen.Reduce.Data where

import Gen.Imports
import Gen.IO.RunResult
import Gen.IO.Toolchain   (KindI, StatusI, ToolchainOutput (..))
import Gen.Reduce.Random
import System.Random.TF
import Gen.Helpers.MonadNote

import qualified Text.PrettyPrint as Pr
import qualified Text.PrettyPrint as P

type RRR a = forall m
  . (MonadIO m, MonadState RState m, RndGen m, MonadR m, MonadDB m, MonadLog m, MonadNote m)
  => m a

data RConfig = RConfig
    { oErrKind_         :: KindI
    , oErrStatus_       :: StatusI
    , oErrChoices_      :: Maybe FilePath
    , outputDir_        :: FilePath
    , specDir_          :: FilePath
    , cores_            :: Int
    , specTime_         :: Int
    , printState_       :: Bool

    -- def Initialised
    , binariesDirectory_ :: Maybe FilePath
    , toolchainOutput_   :: ToolchainOutput

    , deletePassing_      :: Bool
    , resultsDB_dir       :: Maybe FilePath

    , alwaysCompact_      :: Bool
    } deriving Show

data RState = RState
    { rconfig             :: RConfig
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
          ,alwaysCompact_      = False
          ,printState_         = True
          }

instance Default RState where
    def =  RState{rconfig             = def
                 ,resultsDB_          = def
                 ,mostReduced_        = Nothing
                 ,otherErrors_        = []
                 ,mostReducedChoices_ = error "set mostReducedChoices_=oErrChoices_"
                 ,timeLeft_           = Nothing
                 }


mkrGen :: Int -> TFGen
mkrGen = mkTFGen


data EState = EState

newEState :: EState
newEState = EState


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

addLog :: MonadLog m => String -> [Doc] ->  m ()
addLog t ds = logDebugVerbose $ hang (pretty t) 4 (vcat ds)

addLog2 :: MonadLog m => String -> [Doc] ->  m ()
addLog2 t ds = logInfo $ hang (pretty t) 4 (vcat ds)

rrError :: String -> [Doc] -> m a
rrError title docs = do
  error . show $ ( P.text $ padRight 15 ' ' title  )
      P.$+$ (nest 4 $ vcat (docs ))
      P.$+$ ""
