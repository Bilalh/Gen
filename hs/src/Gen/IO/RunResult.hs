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

newtype ResultsDB = ResultsDB (HashMap Int RunResult)
  deriving Show

instance Default ResultsDB where
    def = ResultsDB (H.empty)

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

instance Pretty RunResult where
    pretty = pretty . groom


instance FromJSON RunResult
instance ToJSON   RunResult

instance ToJSON ResultsDB where
    toJSON (ResultsDB m) = toJSON . H.toList $ m

instance FromJSON ResultsDB where
    parseJSON  val = (ResultsDB . H.fromList) <$> parseJSON val


-- | Cache the Spec
storeInDB :: (MonadDB m, MonadIO m) => Spec -> RunResult  -> m ()
storeInDB sp r = do
  let newHash = hash sp
  getsDb >>=  \(ResultsDB m) -> do
      let newDB = H.insert newHash r m
      -- liftIO $ putStrLn . show . vcat $ ["Storing " <+> pretty newHash
      --                                   ,"for" <+> pretty sp
      --                                   , "db" <+> prettyArr (H.toList $ newDB)
      --                                   ]
      putsDb (ResultsDB newDB)



-- | Check if the spec's hash is contained, return the result if it is
checkDB :: (MonadDB m, MonadIO m)
        => KindI
        -> StatusI
        -> Spec
        -> m (Maybe RunResult)
checkDB kind status newE= do
  let newHash = hash newE
  getsDb >>=  \(ResultsDB m) ->
      case newHash `H.lookup` m of
        Nothing              -> do
                -- liftIO $ putStrLn . show . vcat $
                --            [ "No result found for hash" <+>  pretty newHash
                --            , nn "spec" newE
                --            , "db" <+> prettyArr (H.toList $ m)
                --            -- , nn "gromed" (groom newE)
                --            ]
                return Nothing
        Just r@OurError{} | not (sameError r) -> do
          docError [pretty $line, "OurError is not the same"
                   , nn "kind" kind
                   , nn "status" status
                   , nn "result" r]

        Just r@Passing{}   -> return (Just r)
        Just r@OurError{}  -> return (Just r)
        Just r@StoredError{} | not (sameError r) -> return Nothing
        Just StoredError{..} -> do
          out <- getOutputDirectory
          outDir <- sortByKindStatus >>= \case
            False -> return $ (out </> takeBaseName resDirectory_ )
            True -> return  $ (out </> show resErrKind_
                                   </> show resErrStatus_
                                   </> takeBaseName resDirectory_ )

          let newChoices = replaceDirectory resErrChoices_ outDir
          let err = OurError{resDirectory_= outDir, resErrChoices_=newChoices, .. }

          db_dir <-getDbDirectory >>= \case
                    Just df -> return df
                    Nothing -> $(neverNote "Using an StoredError without knowing the filepath")

          liftIO $ doesDirectoryExist outDir >>= \case
            True  -> return $ Just err
            False -> do
              liftIO $ copyDirectory (db_dir </> resDirectory_)  outDir
              return $ Just err


  where
    sameError :: RunResult -> Bool
    sameError Passing{} = True
    sameError r = resErrKind_ r == kind && resErrStatus_ r == status


-- | Save the DB if given a filepath
saveDB :: (MonadDB m, MonadIO m) => Bool -> m ()
saveDB onlyPassing = do
  db  <- getsDb
  out <-  getDbDirectory
  saveDB_ onlyPassing out db


-- | Save the DB if given a filepath
saveDB_ :: MonadIO m => Bool -> Maybe FilePath -> ResultsDB -> m ()
saveDB_ _ Nothing  _    = return ()
saveDB_ onlyPassing (Just dir) (ResultsDB db) = do
  liftIO $ createDirectoryIfMissing True dir
  let dbUse = H.filter (removeErrors onlyPassing) db

  ndb <- liftIO $ H.traverseWithKey f dbUse
  liftIO $ writeToJSON (dir </> "db.json") (ResultsDB ndb)

  where
    removeErrors False _        = True
    removeErrors True Passing{} = True
    removeErrors _  _           = False

    f _ OurError{..} = do
      liftIO $ putStrLn ""
      let newDir = takeBaseName resDirectory_
      copyDirectory resDirectory_ (dir </> newDir)
      let newChoices = replaceDirectory resErrChoices_ newDir
      return $ StoredError{resDirectory_= newDir, resErrChoices_=newChoices, ..}

    f _ x = return x
