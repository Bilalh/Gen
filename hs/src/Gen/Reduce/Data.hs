{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Reduce.Data where

import Gen.Helpers.Log
import Gen.Imports
import Gen.IO.RunResult
import Gen.IO.Toolchain          (KindI, StatusI, ToolchainOutput (..))
import System.Random
import System.Random.TF

import qualified Text.PrettyPrint    as Pr

type RR a = StateT RState IO a

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
    , rlogs_              :: LogsTree
    , timeLeft_           :: Maybe Int
    , resultsDB_          :: ResultsDB
    } deriving (Show)


instance Pretty RConfig where
    pretty = pretty . groom

instance Pretty RState where
    pretty RState{..} =
        "RState" <+> Pr.braces (
            Pr.sep
                [ nn "rconfig"  rconfig
                , nn "mostReduced_" mostReduced_
                , nn "mostReducedChoices_" mostReducedChoices_
                , nn "timeLeft_"        timeLeft_
                , nn "otherErrors_" (prettyArr otherErrors_)
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
                 ,rlogs_              = LSEmpty
                 ,mostReducedChoices_ = error "set mostReducedChoices_=oErrChoices_"
                 ,timeLeft_           = Nothing
                 }


mkrGen :: Int -> TFGen
mkrGen = mkTFGen


class (Monad r, Applicative r) => HasGen r where
  getGen :: r TFGen
  putGen :: TFGen -> r ()

instance HasGen (StateT RState IO) where
  getGen = gets rgen_
  putGen g = modify $ \st -> st{rgen_=g }


chooseR :: (Random a, HasGen m) => (a,a) -> m a
chooseR ins = do
    rgen  <- getGen
    let (num,rgen') = randomR ins rgen
    putGen rgen'
    return num

-- | Randomly chooses one of the elements
oneofR :: (HasGen m, HasLogger m)  => [a] -> m a
oneofR [] = rrError "oneOfR used with empty list" []
oneofR gs = do
  ix <- chooseR (0,length gs - 1)
  return $ gs `at` ix



data EState = EState
  { spec_  :: Spec
  , sgen_  :: TFGen
  , elogs_ :: LogsTree
  }

type ES a = StateT EState Identity a

instance HasGen (StateT EState Identity) where
  getGen   = gets sgen_
  putGen g = modify $ \st -> st{sgen_=g }

newEState :: HasGen m => Spec -> m EState
newEState sp = do
  newSeed <- chooseR (0 :: Int ,2^(24:: Int) )
  return $ EState{spec_=sp,sgen_=mkrGen newSeed,elogs_=LSEmpty}

newEStateWithSeed :: Int -> Spec -> EState
newEStateWithSeed seed sp = do
  EState{spec_=sp,sgen_=mkrGen seed,elogs_=LSEmpty}



instance HasGen (StateT TFGen Identity) where
  getGen  = get
  putGen  = put

data WithGen a = WithGen { withGen_gen :: TFGen
                         , withGen_val :: a
                         }

withGen_new :: HasGen m => a -> m (WithGen a)
withGen_new a = do
  g <- getGen
  return $ WithGen{withGen_gen=g, withGen_val = a}

withGen_put :: forall (m :: * -> *) t.
               MonadState (WithGen t) m =>
               t -> m ()
withGen_put val = do
  modify $ \st -> st{withGen_val = val }

instance (Monad m, Applicative m) => HasLogger (StateT (WithGen a) m) where
    getLog   = return LSEmpty
    putLog _ = return ()


instance (Monad m, Applicative m) => HasGen (StateT (WithGen a) m) where
  getGen   = gets withGen_gen
  putGen g = modify $ \st -> st{withGen_gen = g }


instance HasLogger (StateT RState IO)  where
    getLog = gets rlogs_
    putLog lg = modify $ \st -> st{ rlogs_=lg}


instance HasLogger (StateT EState Identity)  where
    getLog = gets elogs_
    putLog lg = modify $ \st -> st{ elogs_=lg}


instance (HasLogger (StateT EState (IdentityT (StateT RState IO)))) where
  getLog = gets elogs_
  putLog lg = modify $ \st -> st{ elogs_=lg}

instance (HasGen (StateT EState (IdentityT (StateT RState IO)))) where
  getGen   = gets sgen_
  putGen g = modify $ \st -> st{sgen_=g }


instance Monad m => MonadDB (StateT RState  m) where
  getsDb             = gets  resultsDB_
  putsDb db          = modify $ \st -> st{resultsDB_=db}
  getDbDirectory     = gets rconfig >>= return . resultsDB_dir
  getOutputDirectory = gets rconfig >>= return . outputDir_


class Monad m => MonadR m where
  getRconfig        :: m RConfig
  processOtherError :: ErrData -> m ()
  getChoicesToUse   :: m (Maybe FilePath)

instance Monad m => MonadR (StateT RState m) where
  getRconfig          = gets rconfig
  processOtherError r = modify $ \st -> st{otherErrors_ =r : otherErrors_ st }
  getChoicesToUse     = gets mostReducedChoices_
