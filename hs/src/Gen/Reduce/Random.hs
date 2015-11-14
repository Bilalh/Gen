{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Gen.Reduce.Random where

import Gen.Imports
import System.Random.TF
import System.Random

type RandomState = TFGen


class Monad r => RndGen r where
  getGen :: r TFGen
  putGen :: TFGen -> r ()

newtype RndGenM m a = RndGenM (StateT RandomState m a)
    deriving ( Functor, Applicative, Monad
             , MonadFail
             , MonadLog
             , MonadTrans
             , MonadState RandomState
             , MonadIO
             )

instance RndGen m => RndGen (StateT st m) where
    getGen   = lift $ getGen
    putGen g = lift $ (putGen g)

instance (RndGen m, Monoid w) => RndGen (WriterT w m) where
    getGen   = lift $ getGen
    putGen g = lift $ (putGen g)

instance RndGen m => RndGen (ReaderT r m) where
    getGen   = lift $ getGen
    putGen g = lift $ (putGen g)

instance RndGen m => RndGen (IdentityT m) where
    getGen   = lift $ getGen
    putGen g = lift $ (putGen g)

instance RndGen m => RndGen (ExceptT m) where
    getGen   = lift $ getGen
    putGen g = lift $ (putGen g)


instance Monad m => RndGen (RndGenM m) where
    getGen   = get
    putGen g = modify $ \_ -> g


newRGen :: Int -> TFGen
newRGen = mkTFGen

runRndGen :: Monad m => Int -> RndGenM m a -> m a
runRndGen i (RndGenM comp) = evalStateT comp initState
    where initState = mkTFGen i


chooseR :: (Random a, RndGen m) => (a,a) -> m a
chooseR ins = do
    rgen  <- getGen
    let (num,rgen') = randomR ins rgen
    putGen rgen'
    return num

-- | Randomly chooses one of the elements
oneofR :: (RndGen m)  => [a] -> m a
oneofR [] = error "oneOfR used with empty list"
oneofR gs = do
  ix <- chooseR (0,length gs - 1)
  return $ gs `at` ix
