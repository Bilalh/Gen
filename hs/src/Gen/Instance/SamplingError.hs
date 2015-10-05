{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             DeriveTraversable #-}

module Gen.Instance.SamplingError where

import Conjure.Language.Pretty
import Control.Monad           (fail)
import Gen.Imports             hiding (fail)
import System.Exit             (exitFailure)
import System.IO               as X (hPutStrLn, stderr)

import qualified Conjure.Prelude as Prelude (MonadFail (..))

data SamplingErr = ErrDontCountIteration Doc
                 | ErrNoValuesLeft Doc
                 | ErrFailedToGenerateParam Doc
                 | ErrRace Doc
                 | ErrGather Doc
  deriving (Eq, Show, Typeable)

instance  Pretty SamplingErr  where
    pretty (ErrDontCountIteration d)    = hang "ErrDontCountIteration" 4 d
    pretty (ErrNoValuesLeft d)          = hang "ErrNoValuesLeft" 4 d
    pretty (ErrFailedToGenerateParam d) = hang "ErrFailedToGenerateParam" 4 d
    pretty (ErrRace d)                  = hang "ErrRace" 4 d
    pretty (ErrGather d)                = hang "ErrGather" 4 d


class Monad m => MonadSamplingError m where
    sampingErr :: SamplingErr -> m a

instance MonadSamplingError (Either SamplingErr) where
    sampingErr msgs = do
      Left $ msgs

instance MonadSamplingError IO where
    sampingErr msgs =
        case sampingErr msgs of
            Left err -> hPutStrLn stderr (renderNormal (err :: SamplingErr )) >> exitFailure
            Right x  -> return x

instance MonadSamplingError m => MonadSamplingError (IdentityT m) where
    sampingErr = lift . sampingErr

instance MonadSamplingError m => MonadSamplingError (MaybeT m) where
    sampingErr = lift . sampingErr

instance MonadSamplingError m => MonadSamplingError (ExceptT m) where
    sampingErr = lift . sampingErr

instance MonadSamplingError m => MonadSamplingError (StateT st m) where
    sampingErr = lift . sampingErr

instance (MonadSamplingError m, Monoid w) => MonadSamplingError (WriterT w m) where
    sampingErr = lift . sampingErr

instance MonadSamplingError m => MonadSamplingError (ReaderT r m) where
    sampingErr = lift . sampingErr

-- | This is to run a MonadSamplingError. Everything else should lift.
newtype SamplingErrorT m a = SamplingErrorT { runSamplingErrorT :: m (Either SamplingErr a) }

runSampingError :: SamplingErrorT Identity a -> Either SamplingErr a
runSampingError ma = runIdentity (runSamplingErrorT ma)

instance (Functor m) => Functor (SamplingErrorT m) where
    fmap f = SamplingErrorT . fmap (fmap f) . runSamplingErrorT

instance (Functor m, MonadFail m) => Applicative (SamplingErrorT m) where
    pure = return
    (<*>) = ap

instance (MonadFail m) => Monad (SamplingErrorT m) where
    return a = SamplingErrorT $ return (Right a)
    m >>= k = SamplingErrorT $ do
        a <- runSamplingErrorT m
        case a of
            Left e -> return (Left e)
            Right x -> runSamplingErrorT (k x)
    fail = lift . fail

instance (MonadIO m, MonadFail m) => MonadIO (SamplingErrorT m) where
    liftIO comp = SamplingErrorT $ do
        res <- liftIO comp
        return (Right res)

instance MonadTrans SamplingErrorT where
    lift comp = SamplingErrorT $ do
        res <- comp
        return (Right res)

instance MonadFail m => MonadFail (SamplingErrorT m) where
    fail = lift . Prelude.fail

instance MonadFail m => MonadSamplingError (SamplingErrorT m) where
    sampingErr msgs = SamplingErrorT $ return $ sampingErr msgs


_runFunc :: IO ()
_runFunc = do
  x <- runSamplingErrorT (check "d")
  print x

  where
  check :: (MonadIO m, MonadSamplingError m) => FilePath -> m Int
  check _ = do
    y <- bb
    liftIO $ print y
    x <- aa
    liftIO $ putStrLn "dsds"
    return $ y + x

  bb :: (MonadSamplingError m) => m Int
  bb = return 44


  aa :: (MonadSamplingError m) => m Int
  aa = sampingErr (ErrGather "f")
