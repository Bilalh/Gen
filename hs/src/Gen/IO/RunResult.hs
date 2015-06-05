{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.IO.RunResult where

import Data.HashMap.Strict (HashMap)
import Gen.Imports
import Gen.IO.Formats      (copyDirectory, writeToJSON)
import Gen.IO.Toolchain    (KindI, StatusI)
import System.FilePath     (replaceDirectory, takeBaseName)

import qualified Data.HashMap.Strict as H

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

-- | Results of running a spec for caching
class Monad m => MonadDB m where
    getsDb   :: m ResultsDB
    putsDb   :: ResultsDB -> m ()

instance Pretty RunResult where
    pretty = pretty . groom


instance FromJSON RunResult
instance ToJSON   RunResult

instance ToJSON (HashMap Int RunResult) where
    toJSON  = toJSON . H.toList

instance FromJSON (HashMap Int RunResult) where
    parseJSON  val = H.fromList <$> parseJSON val


-- | Cache the Spec
storeInDB :: (MonadDB m, MonadIO m) => Spec -> RunResult  -> m ()
storeInDB sp r = do
  let newHash = hash sp
  getsDb >>=  \m -> do
      let newDB = H.insert newHash r m
      -- liftIO $ putStrLn . show . vcat $ ["Storing " <+> pretty newHash
      --                                   ,"for" <+> pretty sp
      --                                   , "db" <+> prettyArr (H.toList $ newDB)
      --                                   ]
      putsDb newDB



saveDB :: (MonadDB m, MonadIO m) => Bool -> Maybe FilePath -> m ()
saveDB b may= do
  db <-getsDb
  saveDB_ b may db


-- | Save the DB if given a filepath
saveDB_ :: MonadIO m => Bool -> Maybe FilePath -> ResultsDB -> m ()
saveDB_ _ Nothing  _    = return ()
saveDB_ onlyPassing (Just dir) db = do
  liftIO $ createDirectoryIfMissing True dir
  let dbUse = H.filter (removeErrors onlyPassing) db

  ndb <- liftIO $ H.traverseWithKey f dbUse
  liftIO $ writeToJSON (dir </> "db.json") ndb

  where
    removeErrors False _        = True
    removeErrors True Passing{} = True
    removeErrors _  _           = False

    f _ OurError{..} = do
      liftIO $ putStrLn ""
      let newDir = takeBaseName resDirectory_
      copyDirectory resDirectory_ (dir </>
                                       newDir)
      let newChoices = replaceDirectory resErrChoices_ newDir
      return $ StoredError{resDirectory_= newDir, resErrChoices_=newChoices, ..}

    f _ x = return x
