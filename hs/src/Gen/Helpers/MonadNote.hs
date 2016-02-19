{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances #-}
module Gen.Helpers.MonadNote  where

import Gen.Imports
import Gen.Reduce.Random
import qualified Pipes

class Monad m => MonadNote m where
  note :: Doc -> m ()

newtype NoteT m a = NoteT (WriterT [Doc] m a)
    deriving ( Functor, Applicative, Monad
             , MonadLog
             , MonadTrans
             , MonadWriter [Doc]
             , MonadIO
             )


instance MonadNote IO where
  note = putStrLn . renderSized 79

instance Monad m => MonadNote (NoteT m) where
  note msg =  NoteT $ tell [msg]

instance  (MonadNote m, Monoid w) => MonadNote (WriterT w m) where
    note msg  = lift $ note msg

instance MonadNote m => MonadNote (ReaderT st m) where
    note msg  = lift $ note msg

instance MonadNote m => MonadNote (StateT st m) where
    note msg  = lift $ note msg

instance MonadNote m => MonadNote (IdentityT m) where
    note msg  = lift $ note msg

instance MonadNote m => MonadNote (ExceptT m) where
    note msg  = lift $ note msg


instance MonadNote m => MonadNote (Pipes.Proxy a b c d m) where
    note msg  = lift $ note msg

instance MonadNote m => MonadNote (RndGenM m) where
    note msg  = lift $ note msg

instance RndGen m => RndGen (NoteT m) where
    getGen   = lift $ getGen
    putGen g = lift $ (putGen g)



runNoteT :: Monad m => NoteT m a -> m (a, [Doc])
runNoteT (NoteT ma) = do
  (a, logs) <- runWriterT ma
  return (a, logs)

noteFormat :: MonadNote m => Doc -> [Doc] -> m ()
noteFormat tx pr = note $ hang tx 4 (vcat  pr)

noteFormat1 :: MonadNote m => Doc -> Doc -> m ()
noteFormat1 tx pr = noteFormat tx [pr]

ignoreNotes :: Monad m => IdentityT m a -> m a
ignoreNotes = runIdentityT

logsToFile :: MonadIO m => FilePath -> [Doc]  -> m ()
logsToFile out lgs = do
   let s = renderSized 120  . vcat $ lgs
   liftIO $ writeFile out s

ff :: MonadNote m => m Int
ff = do
  noteFormat "State" [ ]
  noteFormat "Start" ["sdsdsd" :: Doc]
  return 2

gg :: IO ()
gg = do
  (res,lgs) <- runNoteT ff
  putStrLn . renderSized 100  . vcat $ lgs
  print res