{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.IO.RunResult where

import Data.HashMap.Strict (HashMap)
import Gen.Imports
import Gen.IO.Formats      (copyDirectory, writeToJSON,readFromJSONMay)
import Gen.IO.Toolchain    (KindI, StatusI)
import System.FilePath     (replaceDirectory, takeBaseName)

import qualified Data.HashMap.Strict as H

-- | Results of running a spec for caching
class Monad m => MonadDB m where
    getsDb             :: m ResultsDB
    putsDb             :: ResultsDB -> m ()
    -- | The directory to save the database in
    getDbDirectory     :: m (Maybe FilePath)
    -- | The directory where specs are ran
    getOutputDirectory :: m FilePath

    -- | True if the errors should be sorted a kind/status/id
    -- | Mainly for generate
    sortByKindStatus :: m Bool
    sortByKindStatus = return False


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
    } deriving (Eq, Show, Data, Typeable, Generic)

type Hash = Int
type Time = Int
data ResultsDB = ResultsDB{
      resultsPassing :: Mapped Hash Time
    , resultsErrors  :: Mapped (Hash, KindI, StatusI) RunResult
    }
  deriving (Eq, Show, Data, Typeable, Generic)

newtype Mapped a b = Mapped (HashMap a b)
    deriving (Eq, Show, Data, Typeable, Generic)


instance Default ResultsDB where
    def = ResultsDB{resultsPassing = def
                   ,resultsErrors  = def
                   }

instance FromJSON ResultsDB
instance ToJSON ResultsDB

instance Pretty RunResult where
    pretty = pretty . groom

instance FromJSON RunResult
instance ToJSON   RunResult

instance Default (Mapped a b) where
    def = Mapped $ H.empty

instance (ToJSON a, ToJSON b, Hashable a, Eq a)
    => ToJSON (Mapped a b) where
    toJSON (Mapped m) = toJSON . H.toList $ m

instance (FromJSON a, FromJSON b, Hashable a, Eq a)
    => FromJSON (Mapped a b) where
    parseJSON  val = (Mapped . H.fromList) <$> parseJSON val





-- | Cache the Spec
storeInDB :: (MonadDB m, MonadIO m) => Spec -> RunResult  -> m ()
storeInDB sp r = do
  let newHash = hash sp
  ndb <- getsDb >>= \db -> case (r,db) of
    (Passing{timeTaken_=t}, ResultsDB{resultsPassing=Mapped m} ) ->
      return $ db{resultsPassing = Mapped $ H.insertWith max newHash t m}

    (err, ResultsDB{resultsErrors=Mapped m}) -> do
      let (k,s) = (resErrKind_ err, resErrStatus_ err)
      -- TODO choose which one to keep
      return $ db{resultsErrors = Mapped $ H.insertWith comp (newHash,k,s) err m}

  putsDb ndb

  where
    comp e1 e2 = if timeTaken_ e1 > timeTaken_ e2 then e1 else e2

-- | Check if the spec's hash is contained, return the result if it is
checkDB :: (MonadDB m, MonadIO m)
        => KindI
        -> StatusI
        -> Spec
        -> m (Maybe RunResult)
checkDB kind status newE= do
  let newHash = hash newE
  getsDb >>= \ResultsDB{resultsPassing = Mapped m1, resultsErrors = Mapped m2 } -> do
    case newHash `H.lookup` m1 of
      Just i -> return . Just . Passing $ i
      Nothing -> do
        case (newHash, kind, status) `H.lookup` m2 of
          Nothing -> return Nothing
          Just Passing{} -> $(neverNote "Error Db contains passing")
          Just r@OurError{}  -> return (Just r)
          Just StoredError{..} -> do
            out <- getOutputDirectory
            outDir <- sortByKindStatus >>= \case
              False -> return $ (out </> takeBaseName resDirectory_ )
              True -> return  $ (out </> show resErrKind_
                                     </> show resErrStatus_
                                     </> takeBaseName resDirectory_ )

            let newChoices = replaceDirectory resErrChoices_ outDir
            let err = OurError{resDirectory_= outDir, resErrChoices_=newChoices, .. }

            db_dir <- getDbDirectory >>= \case
              Just df -> return df
              Nothing -> $(neverNote "Using an StoredError without knowing the filepath")

            liftIO $ doesDirectoryExist outDir >>= \case
              True  -> return $ Just err
              False -> do
                liftIO $ copyDirectory (db_dir </> resDirectory_)  outDir
                return $ Just err




-- | Save the DB if given a filepath
writeDB :: (MonadDB m, MonadIO m) => Bool -> m ()
writeDB onlyPassing = do
  db  <- getsDb
  out <-  getDbDirectory
  writeDB_ onlyPassing out db


-- | Save the DB if given a filepath
writeDB_ :: MonadIO m => Bool -> Maybe FilePath -> ResultsDB -> m ()
writeDB_ _ Nothing  _    = return ()
writeDB_ onlyPassing (Just dir)
         ResultsDB{resultsPassing = Mapped m1, resultsErrors = Mapped m2 } = do
  liftIO $ createDirectoryIfMissing True dir

  nm2 <- liftIO $ H.traverseWithKey f (if onlyPassing then H.empty else m2)
  liftIO $ writeToJSON (dir </> "db.json")
           (ResultsDB{resultsPassing=Mapped m1,resultsErrors=Mapped nm2})

  where
    f _ OurError{..} = do
      liftIO $ putStrLn ""
      let newDir = takeBaseName resDirectory_
      copyDirectory resDirectory_ (dir </> newDir)
      let newChoices = replaceDirectory resErrChoices_ newDir
      return $ StoredError{resDirectory_= newDir, resErrChoices_=newChoices, ..}

    f _ x = return x


giveDb ::  Maybe FilePath -> Maybe FilePath -> IO ResultsDB
giveDb dir passing = do
  r@ResultsDB{resultsPassing=Mapped m2}  <- getData $  (</> "db.json") <$> dir
  ResultsDB{resultsPassing=Mapped extra} <- getData passing
  return r{ resultsPassing = Mapped $ H.unionWith max m2 extra }

  where
    getData :: Maybe FilePath -> IO ResultsDB
    getData Nothing   = return $ def
    getData (Just fp) =
        readFromJSONMay fp >>= \case
                  Nothing  -> return $ def
                  (Just x) -> return x
