{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, ViewPatterns #-}
module Gen.IO.RunResult where

import Data.Coerce(coerce)
import Data.HashMap.Strict  (HashMap)
import Gen.Imports
import Gen.Instance.Point
import Gen.IO.Formats       (copyDirectory, readFromJSONMay, writeToJSON)
import Gen.IO.Toolchain     (KindI, StatusI)
import Gen.IO.ToolchainData (KindI (..), StatusI (..))
import System.FilePath      (replaceDirectory, takeBaseName)

import qualified Data.HashMap.Strict as H
import qualified Data.IntSet         as I


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

    useSkipped :: m Bool
    useSkipped = return False

data ErrData = ErrData
    { kind      :: KindI
    , status    :: StatusI
    , choices   :: FilePath
    , specDir   :: Directory
    , timeTaken :: Time
    } deriving (Eq, Show, Data, Typeable, Generic)

instance FromJSON ErrData
instance ToJSON   ErrData

instance Pretty ErrData where
    pretty = pretty . groom


data RunResult =
      OurError    ErrData
    | StoredError ErrData
    | Passing Time
    deriving (Eq, Show, Data, Typeable, Generic)

instance Pretty RunResult where
    pretty = pretty . groom

instance FromJSON RunResult
instance ToJSON   RunResult


data ResultsDB = ResultsDB{
      resultsPassing :: Mapped (SpecHash,ParamHash) Time
    , resultsErrors  :: Mapped (SpecHash, ParamHash, KindI, StatusI) RunResult
    , resultsSpecs   :: I.IntSet -- | Hashes of specs that have be run
    , resultsSkipped :: I.IntSet -- | Hashes of the specs or params to skip
    }
  deriving (Eq, Show, Data, Typeable, Generic)

instance Default ResultsDB where
    def = ResultsDB{resultsPassing = def
                   ,resultsErrors  = def
                   ,resultsSpecs   = def
                   ,resultsSkipped = def
                   }

instance FromJSON ResultsDB
instance ToJSON ResultsDB

instance Monoid ResultsDB  where
    mempty = def

    mappend a b =
      ResultsDB{resultsPassing = resultsPassing a `mappend` resultsPassing b
               ,resultsErrors  = resultsErrors a  `mappend` resultsErrors b
               ,resultsSpecs   = resultsSpecs a   `mappend` resultsSpecs b
               ,resultsSkipped = resultsSkipped a `mappend` resultsSkipped b
               }

newtype Mapped a b = Mapped (HashMap a b)
    deriving (Eq, Show, Data, Typeable, Generic)

instance Default (Mapped a b) where
    def = Mapped $ H.empty

instance (Eq a, Hashable a) => Monoid (Mapped a b) where
    mempty  = def
    mappend (Mapped a) (Mapped b) = Mapped (H.union a b)
    mconcat xs = Mapped $ H.unions (coerce xs :: [HashMap a b])

instance (ToJSON a, ToJSON b, Hashable a, Eq a)
    => ToJSON (Mapped a b) where
    toJSON (Mapped m) = toJSON . H.toList $ m

instance (FromJSON a, FromJSON b, Hashable a, Eq a)
    => FromJSON (Mapped a b) where
    parseJSON  val = (Mapped . H.fromList) <$> parseJSON val


viewResultError :: MonadFail m => RunResult -> m ErrData
viewResultError (OurError x)    = return x
viewResultError (StoredError x) = return x
viewResultError Passing{}       = fail "viewError, Passing is not an Error"

viewResultErrorM :: MonadFail m => Maybe RunResult -> m ErrData
viewResultErrorM (Just (OurError x))    = return x
viewResultErrorM (Just (StoredError x)) = return x
viewResultErrorM _                      = fail "viewErrorM, not an Error"

viewResultTime :: RunResult -> Time
viewResultTime (OurError x)    = timeTaken x
viewResultTime (StoredError x) = timeTaken x
viewResultTime (Passing t)     = t



-- | Cache the Spec
storeInDB :: (MonadDB m, MonadIO m) => Spec -> Maybe Point -> RunResult  -> m ()
storeInDB sp mayPoint r = do
  let specHash  = hashDoc sp
  let paramHash = hashDoc mayPoint
  ndb <- getsDb >>= \db -> case (r,db) of
    (Passing t, ResultsDB{resultsPassing=Mapped m, resultsSpecs} ) ->
      return $ db{resultsPassing = Mapped $ H.insertWith max (specHash, paramHash) t m
                 ,resultsSpecs   = specHash `I.insert` resultsSpecs }

    (e@(OurError ErrData{..}), ResultsDB{resultsErrors=Mapped m, resultsSpecs})-> return $
      db{resultsErrors = Mapped $ doError kind status specHash paramHash m e
        ,resultsSpecs  = specHash `I.insert` resultsSpecs }

    (e@(StoredError ErrData{..}), ResultsDB{resultsErrors=Mapped m,resultsSpecs})->return$
      db{resultsErrors = Mapped $ doError kind status specHash paramHash m e
        ,resultsSpecs = specHash `I.insert` resultsSpecs }

  putsDb ndb

  where
    -- Store extra version to account for any types
    doError kind status specHash paramHash m e =
        H.insertWith comp (specHash, paramHash, kind,     status ) e
      $ H.insertWith comp (specHash, paramHash, KindAny_, status ) e
      $ H.insertWith comp (specHash, paramHash, kind,     StatusAny_) e
      $ H.insertWith comp (specHash, paramHash, KindAny_, StatusAny_) e
      $ m


    comp e1@(viewResultTime -> t1) e2@(viewResultTime -> t2) = if t1 > t2 then e1 else e2

inDB :: (MonadDB m) => Spec -> Maybe Point -> m Bool
inDB sp mayPoint = do
  let specHash  = hashDoc sp
  let paramHash = hashDoc mayPoint
  getsDb >>= \ResultsDB{resultsPassing = Mapped m1, resultsErrors = Mapped m2
                       , resultsSkipped} -> do
   case (specHash, paramHash) `H.lookup` m1 of
     Just{}  -> return True
     Nothing ->
       case any (\(hs,hp,_,_) -> (hs,hp) == (specHash,paramHash)) $  H.keys m2 of
         True  -> return True
         False -> checkWithSkipped specHash paramHash resultsSkipped

specInDB :: (MonadDB m) => Spec -> m Bool
specInDB sp =
  getsDb >>= \ResultsDB{resultsSpecs} ->
    return $ hashDoc sp `I.member` resultsSpecs


-- | Check if the spec's hash is contained, return the result if it is
checkDB :: (MonadDB m, MonadIO m)
        => KindI
        -> StatusI
        -> Spec
        -> Maybe Point
        -> m (Maybe RunResult)
checkDB kind status sp mayPoint= do
  let specHash  = hashDoc sp
  let paramHash = hashDoc mayPoint

  getsDb >>= \ResultsDB{resultsPassing = Mapped m1, resultsErrors = Mapped m2
                       , resultsSkipped } -> do

    shouldBeSkipped <- checkWithSkipped specHash paramHash resultsSkipped

    let c2 = case (specHash,paramHash) `H.lookup` m1  of
             c@Just{} -> c
             Nothing  -> case shouldBeSkipped of
               True  -> Just 0
               False -> Nothing

    case c2  of
      Just i -> return . Just . Passing $ i
      Nothing -> do
        case (specHash,paramHash, kind, status) `H.lookup` m2 of
          Nothing -> return Nothing
          Just Passing{} -> $(neverNote "Error Db contains passing")
          Just r@OurError{}  -> return (Just r)
          Just (StoredError e@ErrData{specDir,choices}) -> do
            out <- getOutputDirectory
            outDir <- sortByKindStatus >>= \case
              False -> return $ (out </> takeBaseName specDir )
              True -> return  $ (out </> show kind
                                     </> show status
                                     </> takeBaseName specDir )

            let newChoices = replaceDirectory choices outDir
            let err = OurError $ e{specDir= outDir, choices=newChoices}

            db_dir <- getDbDirectory >>= \case
              Just df -> return df
              Nothing -> $(neverNote "Using an StoredError without knowing the filepath")

            liftIO $ doesDirectoryExist outDir >>= \case
              True  -> return $ Just err
              False -> do
                liftIO $ copyDirectory (db_dir </> specDir)  outDir
                return $ Just err

-- | Return true if we should skip the spec or param
checkWithSkipped :: MonadDB m => I.Key -> I.Key -> I.IntSet -> m Bool
checkWithSkipped specHash paramHash resultsSkipped = do
   useS <- useSkipped
   return $ useS && (  specHash  `I.member` resultsSkipped
                    || paramHash `I.member` resultsSkipped
                    )


-- | Save the DB if given a filepath
writeDB :: (MonadDB m, MonadIO m) => Bool -> m ()
writeDB onlyPassing = do
  db  <- getsDb
  out <- getDbDirectory
  writeDB_ onlyPassing out db


-- | Save the DB if given a filepath
writeDB_ :: MonadIO m => Bool -> Maybe FilePath -> ResultsDB -> m ()
writeDB_ _ Nothing  _    = return ()
writeDB_ onlyPassing (Just dir)
    ResultsDB{resultsPassing = Mapped m1, resultsErrors = Mapped m2
             ,resultsSpecs, resultsSkipped } = do
  liftIO $ createDirectoryIfMissing True dir

  nm2 <- liftIO $ H.traverseWithKey f (if onlyPassing then H.empty else m2)
  liftIO $ writeToJSON (dir </> "db.json")
           (ResultsDB{resultsPassing=Mapped m1,resultsErrors=Mapped nm2
                     ,resultsSpecs, resultsSkipped})

  where
    f _ (OurError (ErrData{..})) = do
      -- liftIO $ putStrLn ""
      let newDir = takeBaseName specDir
      copyDirectory specDir (dir </> newDir)
      let newChoices = replaceDirectory choices newDir
      return .StoredError $ ErrData{specDir= newDir, choices=newChoices, ..}

    f _ x = return x

-- | Return a db instance based on the given filepaths
giveDb ::  Maybe Directory -> Maybe FilePath -> IO ResultsDB
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


passingSpecHashes :: Mapped (SpecHash,ParamHash) Time -> [SpecHash]
passingSpecHashes (Mapped m) = map fst . H.keys $ m

errorSpecHashes :: Mapped (SpecHash, ParamHash, KindI, StatusI) RunResult -> [SpecHash]
errorSpecHashes (Mapped m) = map fst4 . H.keys $ m
