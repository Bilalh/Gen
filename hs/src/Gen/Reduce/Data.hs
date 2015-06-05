{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, FlexibleInstances,
             KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Reduce.Data where

import Conjure.Language.Constant
import Data.HashMap.Strict       (HashMap)
import Gen.Imports
import Gen.IO.Toolchain          (KindI, StatusI,ToolchainOutput(..))
import System.Random
import System.Random.TF
import Gen.Helpers.Log

import qualified Data.HashMap.Strict as H
import qualified Text.PrettyPrint    as Pr

etrue, efalse :: Expr
etrue  = ECon (ConstantBool True)
efalse = ECon (ConstantBool False)

type RR a = StateT RState IO a
type Hash = Int

data RState = RState
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
    , timeLeft_           :: Maybe Int
    , totalIsRealTime_    :: Bool
    } deriving (Show)


data RunResult =
    OurError{
      resDirectory_  :: FilePath
    , resErrKind_    :: KindI
    , resErrStatus_  :: StatusI
    , resErrChoices_ :: FilePath
    , timeTaken_     :: Int
    }
    | StoredError{
      resDirectory_  :: FilePath
    , resErrKind_    :: KindI
    , resErrStatus_  :: StatusI
    , resErrChoices_ :: FilePath
    , timeTaken_     :: Int
    }
    | Passing {
      timeTaken_     :: Int
    } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)


type ResultsDB = HashMap Int RunResult

instance FromJSON RunResult
instance ToJSON   RunResult

instance ToJSON (HashMap Int RunResult) where
    toJSON  = toJSON . H.toList

instance FromJSON (HashMap Int RunResult) where
    parseJSON  val = H.fromList <$> parseJSON val



instance Pretty RState where
    pretty RState{..} =
        "RState" <+> Pr.braces (
            Pr.sep
                [ nn "oErrKind_"  oErrKind_
                , nn "oErrStatus_" oErrStatus_
                , nn "oErrChoices_" oErrChoices_

                , nn "outputDir_" outputDir_
                , nn "specDir_" specDir_

                , nn "cores_" cores_
                , nn "specTime_" specTime_
                , nn "binariesDirectory_" binariesDirectory_
                , nn "toolchainOutput_" toolchainOutput_

                , nn "mostReduced_" mostReduced_
                , nn "mostReducedChoices_" mostReducedChoices_
                , nn "otherErrors_" (prettyArr otherErrors_)

                -- , nn "resultsDB_"       resultsDB_
                , nn "resultsDB_dir"    resultsDB_dir
                , nn "timeLeft_"        timeLeft_
                , nn "totalIsRealTime_" totalIsRealTime_

                ])

instance Default RState where
    def =  RState{oErrKind_           = error "need oErrKind_"
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
                 ,timeLeft_           = Nothing
                 ,totalIsRealTime_    = False
                 }

instance Pretty RunResult where
    pretty = pretty . groom


infixl 1 *|
(*|) :: a -> Bool -> Maybe a
a  *| c | c = Just a
_  *| _     = Nothing


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
