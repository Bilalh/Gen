{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Gen.Helpers.MonadNote where

-- This is mainly used in reduction, to save a nicely
-- formatted file of reductions that were tried.

import Gen.Imports
import Gen.Reduce.Random

import qualified Pipes
import qualified Text.PrettyPrint as Pr

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

instance Monad m => MonadNote (IdentityT m) where
    note _ = return ()

instance MonadNote m => MonadNote (ExceptT m) where
    note msg  = lift $ note msg

instance MonadNote m => MonadNote (RndGenM m) where
    note msg  = lift $ note msg

instance RndGen m => RndGen (NoteT m) where
    getGen   = lift $ getGen
    putGen g = lift $ (putGen g)

data LogField =  NoteField Doc
              |  LogField  LogLevel Doc

instance MonadNote m => MonadNote (Pipes.Proxy a b () (Either LogField d) m) where
  note msg =  Pipes.yield (Left (NoteField msg))

instance RndGen m => RndGen (Pipes.Proxy a b () (Either LogField d) m) where
    getGen   = lift $ getGen
    putGen g = lift $ (putGen g)

instance Monad m => MonadLog (Pipes.Proxy a b () (Either (LogField) d) m) where
  log l msg = Pipes.yield (Left (LogField l msg))


runNoteT :: Monad m => NoteT m a -> m (a, [Doc])
runNoteT (NoteT ma) = do
  (a, logs) <- runWriterT ma
  return (a, logs)

runNotePipeIO :: (MonadIO m, MonadNote m)
              => LogLevel -> Pipes.Producer (Either LogField a) m r -> m r
runNotePipeIO clvl logger  = Pipes.runEffect $ Pipes.for logger each
  where
  each (Left (NoteField msg)) = do
    liftIO $ putStrLn $ Pr.renderStyle (Pr.style { Pr.lineLength = 120 }) msg
    lift $ note msg
  each (Left (LogField lvl msg)) = when (lvl <= clvl)
    (liftIO $ putStrLn $ Pr.renderStyle (Pr.style { Pr.lineLength = 200 }) msg)
  each _ = return ()


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


_ff :: MonadNote m => m Int
_ff = do
  noteFormat "State" [ ]
  noteFormat "Start" ["sdsdsd" :: Doc]
  return 2

_gg :: IO ()
_gg = do
  (res,lgs) <- runNoteT _ff
  putStrLn . renderSized 100  . vcat $ lgs
  print res