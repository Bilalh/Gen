{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, ViewPatterns #-}
module Gen.IO.RunResult where

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
      rPassing :: Mapped (SpecHash,ParamHash) Time
    , rErrors  :: Mapped (SpecHash, ParamHash, KindI, StatusI) RunResult
    , rSpecs   :: I.IntSet   -- |  SpecHash of rErrors and rPassing for efficent lookup

    -- Hashes of the (spec, param) tuples to skip
    , rSpecsSkipped  :: I.IntSet
    , rParamsSkipped :: I.IntSet
    }
  deriving (Eq, Show, Data, Typeable, Generic)

instance Default ResultsDB where
    def = ResultsDB{rPassing      = def
                   ,rErrors       = def
                   ,rSpecs        = def
                   ,rSpecsSkipped  = def
                   ,rParamsSkipped = I.fromList [hashDoc (Nothing :: Maybe Point)]
                   }

instance FromJSON ResultsDB
instance ToJSON ResultsDB

instance Monoid ResultsDB  where
    mempty = def

    mappend a b =
      ResultsDB{rPassing      = rPassing a      `mappend` rPassing b
               ,rErrors       = rErrors a       `mappend` rErrors b
               ,rSpecs        = rSpecs a        `mappend` rSpecs b
               ,rSpecsSkipped  = rSpecsSkipped a  `mappend` rSpecsSkipped b
               ,rParamsSkipped = rParamsSkipped a `mappend` rParamsSkipped b
               }

newtype Mapped a b = Mapped (HashMap a b)
    deriving (Eq, Show, Data, Typeable, Generic)

instance Default (Mapped a b) where
    def = Mapped $ H.empty

instance (Eq a, Hashable a) => Monoid (Mapped a RunResult) where
    mempty  = def
    mappend (Mapped a) (Mapped b) = Mapped (H.unionWith compRunResult a b)

instance (Eq a, Hashable a) => Monoid (Mapped a Time) where
    mempty  = def
    mappend (Mapped a) (Mapped b) = Mapped (H.unionWith compLess a b)

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


compRunResult :: RunResult -> RunResult  -> RunResult
compRunResult e1@(viewResultTime -> t1) e2@(viewResultTime -> t2) =
  if t1 > t2 then e1 else e2

compLess :: forall a. Ord a => a -> a -> a
compLess t1 t2 =
  if t1 <= t2 then t1 else t2

-- | Cache the Spec
storeInDB :: (MonadDB m, MonadIO m) => Spec -> Maybe Point -> RunResult  -> m ()
storeInDB sp mayPoint r = do
  let specHash  = hashDoc sp
  let paramHash = hashDoc mayPoint
  ndb <- getsDb >>= \db -> case (r,db) of
    (Passing t, ResultsDB{rPassing=Mapped m, rSpecs } ) ->
      return $ db{rPassing = Mapped $ H.insertWith max (specHash, paramHash) t m
                 ,rSpecs  = specHash `I.insert` rSpecs  }

    (e@(OurError ErrData{..}), ResultsDB{rErrors=Mapped m, rSpecs })-> return $
      db{rErrors = Mapped $ doError kind status specHash paramHash m e
        ,rSpecs   = specHash `I.insert` rSpecs  }

    (e@(StoredError ErrData{..}), ResultsDB{rErrors=Mapped m, rSpecs }) -> return $
      db{rErrors = Mapped $ doError kind status specHash paramHash m e
        ,rSpecs  = specHash `I.insert` rSpecs  }

  putsDb ndb

  where
    -- Store extra version to account for Any types
    doError kind status specHash paramHash m e =
        H.insertWith compRunResult (specHash, paramHash, kind,     status ) e
      $ H.insertWith compRunResult (specHash, paramHash, KindAny_, status ) e
      $ H.insertWith compRunResult (specHash, paramHash, kind,     StatusAny_) e
      $ H.insertWith compRunResult (specHash, paramHash, KindAny_, StatusAny_) e
      $ m


specInDB :: (MonadDB m) => Spec -> m Bool
specInDB sp =
  getsDb >>= \ResultsDB{rSpecs, rSpecsSkipped, rParamsSkipped} -> do
    b <- checkWithSkipped (hashDoc sp) (hashDoc (Nothing :: Maybe Point))
                   rSpecsSkipped rParamsSkipped
    return $   b
           ||  hashDoc sp `I.member` rSpecs


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

  getsDb >>= \ResultsDB{ rPassing = Mapped m1, rErrors = Mapped m2 } -> do

    -- shouldBeSkipped <- checkWithSkipped specHash paramHash rSpecsSkipped rParamsSkipped
    let c2 = case (specHash,paramHash) `H.lookup` m1  of
             c@Just{} -> c
             Nothing  -> Nothing
               -- considering skipped as passing would be weird
               -- And we don't have the info to say if it was

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
checkWithSkipped :: MonadDB m => I.Key -> I.Key -> I.IntSet -> I.IntSet -> m Bool
checkWithSkipped specHash paramHash rParamsSkipped rSpecsSkipped = do
   useS <- useSkipped
   return $ useS && (  specHash  `I.member` rSpecsSkipped
                    && paramHash `I.member` rParamsSkipped
                    )

missingToSkipped :: MonadIO m => ResultsDB -> m ResultsDB
missingToSkipped x@ResultsDB{rErrors=Mapped es} = do
  (missing,specHashes, paramHashes) <- (unzip3 .catMaybes) <$>
    mapM findMissing (H.toList es)

  return x{ rErrors        = Mapped (es `H.difference` (H.fromList missing))
          , rSpecs         = rSpecs x I.\\ (I.fromList specHashes)
          , rSpecsSkipped  = rSpecsSkipped x `I.union ` (I.fromList specHashes)
          , rParamsSkipped = rParamsSkipped x `I.union ` (I.fromList paramHashes)
          }

  where
    findMissing y@((sph,ph,_,_), viewResultError -> Just ErrData{..}) = do
      liftIO $ doesDirectoryExist specDir >>= \case
        False -> return $ Just (y,sph, ph)
        True  -> return Nothing

    findMissing _ = return Nothing

-- | Save the DB if given a filepath
writeDb :: (MonadDB m, MonadIO m) => Bool -> m ()
writeDb onlyPassing = do
  db  <- getsDb
  out <- getDbDirectory
  writeDb_ onlyPassing out db


-- | Save the DB if given a filepath
writeDb_ :: MonadIO m => Bool -> Maybe FilePath -> ResultsDB -> m ()
writeDb_ _ Nothing  _    = return ()
writeDb_ onlyPassing (Just dir)
    ResultsDB{ rPassing = Mapped m1, rErrors = Mapped m2
             , rSpecs, rSpecsSkipped, rParamsSkipped} = do
  liftIO $ createDirectoryIfMissing True dir

  nm2 <- liftIO $ H.traverseWithKey f (if onlyPassing then H.empty else m2)
  liftIO $ writeToJSON (dir </> "db.json")
           (ResultsDB{rPassing=Mapped m1,rErrors=Mapped nm2
                     ,rSpecs, rSpecsSkipped, rParamsSkipped})

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
  r@ResultsDB{rPassing=Mapped m2}  <- getData $  (</> "db.json") <$> dir
  ResultsDB{rPassing=Mapped extra} <- getData passing
  return r{ rPassing = Mapped $ H.unionWith max m2 extra }

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

errorHashes :: Mapped (SpecHash, ParamHash, KindI, StatusI) RunResult -> [(SpecHash,ParamHash)]
errorHashes (Mapped m) =  [ (a,b) | (a,b,_,_) <- H.keys m ]

