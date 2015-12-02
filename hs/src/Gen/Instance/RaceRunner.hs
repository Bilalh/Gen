{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
module Gen.Instance.RaceRunner(
    runRace
  , doRace
  , parseRaceResult
  , createParamEssence
  , sampleParamFromMinion
  , RaceTotals(..)
  , getModelOrdering
  , checkPrevious
  , racesTotalCpuTime
  , saveEprimes
  , initDB
  , getPointQuailty
  , paramGenCpuTime
  , readCpuTime
  , runSolve
  , script_lookup1
  , script_lookup
  , runPadded
  , conjureCompact
  , createParamEssence1
  ) where

import Conjure.Language
import Conjure.Language.Expression.DomainSizeOf (domainSizeOf)
import Conjure.Language.NameResolution          (resolveNames)
import Conjure.UI.IO
import Conjure.UI.TypeCheck
import Data.List                                (foldl1')
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField         ()
import Database.SQLite.Simple.FromRow           ()
import Gen.Helpers.InlineLettings
import Gen.Helpers.Str
import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.Point
import Gen.Instance.SamplingError
import Gen.IO.Formats
import Gen.IO.Toolchain                         (runCommand)
import Gen.IO.Toolchain                         (getToolchainDir)
import Shelly                                   (print_stderr, print_stdout,
                                                 runHandles, setenv,
                                                 transferFoldHandleLines)
import System.Directory                         (copyFile)
import System.Environment                       (lookupEnv)
import System.Exit                              (ExitCode (..))
import System.FilePath                          (takeBaseName, takeDirectory)
import System.IO                                (hPutStr, hPutStrLn,
                                                 stderr, stdout,readFile)
import System.IO.Temp                           (withSystemTempDirectory)

import qualified Data.Aeson as A
import qualified Data.Set  as S
import qualified Data.Text as T



runRace :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
        => ParamFP -> m (Either SamplingErr (Quality, RaceTotals))
runRace paramFP = do
  getModelOrdering >>= \case
    Left x -> return $ Left x
    Right ordering -> do
      -- logDebug2 "runRace ordering:" (map pretty ordering)

      p <- readPoint paramFP
      let paramHash = pointHash p
      let paramName = pointName p

      ts <- doRace paramFP ordering
      (Method MCommon{mOutputDir, mMode} _) <- get
      let resDir = mOutputDir </> ("results_" ++ mMode)
      let errorFile = resDir </> ("p-" ++ paramHash ) <.> ".errors"
      liftIO $ print $ nn "errorFile" errorFile
      erred <- liftIO $ doesFileExist errorFile

      case erred of
        True -> do
          (_, info) <- liftIO $ pairWithContents errorFile
          if "Error occurred during initialization of VM" `T.isInfixOf` info then
              return $ Left $ ErrRaceSrInit $ (pretty errorFile <++> pretty info)
          else
              return $ Left $ ErrRace $ (pretty errorFile <++> pretty info)
        False -> do
          totals <- parseRaceResult (paramHash) ts
          logDebug2 "runRace totals:" [pretty totals]

          let quality = calculateParamQuality totals
          logDebug $ "runRace quality:" <+> pretty quality

          timeTaken <- readParamRaceCpuTime ts
          saveQualityToDb paramName paramHash quality timeTaken

          return $ Right (quality,totals)


initDB :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
            => m ()
initDB = do
  (Method MCommon{mOutputDir} _) <- get
  let env = [ ("REPOSITORY_BASE",stringToText mOutputDir)
            ]

  cmd <- script_lookup "instances/db/init_db.sh"
  liftIO $ runPadded "^" env cmd []

saveEprimesQuery :: Query
saveEprimesQuery = [str|
  INSERT INTO Eprimes(eprime,isCompact)
  Values(?, ?)
  |]

saveEprimes :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
            => m ()
saveEprimes= do
  (Method MCommon{mOutputDir, mModelsDir,mCompactName} _) <- get
  let check = case mCompactName of
                Nothing -> (\_ -> Nothing)
                Just c  -> (\x ->  if x == c then Just True else Just False )

  eprimes <- liftIO $ sort <$> getAllFilesWithSuffix ".eprime" mModelsDir
  void $ liftIO $ withConnection (mOutputDir </> "results.db") $ \conn ->
    withTransaction conn $ forM (eprimes) $ \(ep) -> do
      execute conn saveEprimesQuery (takeBaseName ep,  check (takeBaseName ep)  )


getModelOrdering :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
        => m (Either SamplingErr [ FilePath ])
getModelOrdering = do
  (Method MCommon{mOutputDir, mModelsDir} _) <- get
  let dbPath  =  mOutputDir </> "results.db"
  liftIO $ doesFileExist dbPath >>= \case
    False -> return . Left $ ErrDB "DB not found"
    True  -> do
      liftIO $ withConnection dbPath $ \conn -> do
        eprimes :: [[String]] <- query_ conn ("SELECT eprime FROM EprimeOrdering")
        return $ Right [ mModelsDir </> row `at` 0 <.> ".eprime" | row <- eprimes  ]




doRace :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
        => ParamFP -> [FilePath] -> m TimeStamp
doRace paramFP ordering = do
  (Method MCommon{mEssencePath, mOutputDir, mModelTimeout, mMode, mCores} _) <- get
  now <- timestamp


  let args = map stringToText
             [ show now
             , paramFP
             , show mModelTimeout
             , takeDirectory mEssencePath
             ]

  let env = [ ("NUM_JOBS", stringToText (show mCores) )
            , ("USE_MODE",stringToText mMode)
            , ("OUT_BASE_DIR", stringToText mOutputDir)
            ] ++ if null ordering then [] else [ ( "MODELS_TO_USE", stringToText $ intercalate "\n" ordering)]

  cmd <- script_lookup "instances/race.sh"
  liftIO $ runPadded "⌇" env cmd args
  return now


raceResultsQuery :: Query
raceResultsQuery = [str|
  SELECT  1, MinionTimeout, MinionSatisfiable, IsOptimum, isDominated
  FROM TimingsDomination
  Where paramHash = ?
  |]

parseRaceResult :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
                => ParamHash -> TimeStamp -> m RaceTotals
parseRaceResult paramHash ts =do
  (Method MCommon{mEssencePath, mOutputDir, mModelTimeout, mMode} _) <- get

  let args = map stringToText [ paramHash
             , takeDirectory mEssencePath
             ]

  let env = map (second stringToText) [ ("USE_DATE", show ts)
            , ("TOTAL_TIMEOUT", show mModelTimeout)
            , ("USE_MODE", mMode)
            , ("OUT_BASE_DIR", mOutputDir)
            ]

  cmd <- script_lookup "instances/gather_race_results.sh"
  liftIO $ runPadded "❮" env cmd args

  rows <- liftIO $ withConnection (mOutputDir </> "results.db") $ \conn -> do
    rows :: [RaceTotals] <- query conn raceResultsQuery (Only paramHash)
    return rows

  let total = flip foldl1' rows (\(RaceTotals a1 b1 c1 d1 e1)
                                  (RaceTotals a2 b2 c2 d2 e2)
            -> (RaceTotals (a1+a2) (b1+b2) (c1+c2) (d1+d2) (e1+e2)) )

  logDebugVerbose $ "Totals for " <+> pretty paramHash <+> pretty ts
  mapM_ (logDebugVerbose . pretty . groom) rows

  return total


data RaceTotals = RaceTotals
    { tCount                :: Int
    , tMinionTimeout        :: Int
    , tMinionSatisfiable    :: Int
    , tIsOptimum            :: Int
    , tIsDominated          :: Int
    } deriving (Eq, Show, Data, Typeable, Generic)

instance A.FromJSON RaceTotals
instance A.ToJSON RaceTotals

instance Pretty RaceTotals where pretty = pretty . groom

instance FromRow RaceTotals where
    fromRow = RaceTotals <$> field <*> field <*> field <*> field <*> field


racesTotalCpuTime :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
                  => m Double
racesTotalCpuTime = do
  (Method MCommon{mOutputDir, mMode} _) <- get
  let statsDir = mOutputDir </> ("stats_" ++ mMode)
  fps <- liftIO $ getAllFilesWithSuffix ".total_solving_time" statsDir
  times :: [Maybe Double] <- liftIO  $  forM fps $ \fp -> do
             st <- readFile fp
             return $ readMay st
  return . sum . catMaybes $ times

paramGenCpuTime :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
                => m Double
paramGenCpuTime = do
  (Method MCommon{mOutputDir} _) <- get
  statsDir <- liftIO $ doesDirectoryExist (mOutputDir </> "all_sols") >>= \case
              True  -> return (mOutputDir </> "all_sols")
              False -> return (mOutputDir </> "_param_gen")

  fps <- liftIO $ getAllFilesWithSuffix "total.time" statsDir
  times :: [Maybe Double] <- liftIO  $  forM fps $ readCpuTime
  return . sum . catMaybes $ times


readParamRaceCpuTime :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
                     => TimeStamp -> m Double
readParamRaceCpuTime ts = do
  (Method MCommon{mOutputDir, mMode} _) <- get
  let statsDir = mOutputDir </> ("stats_" ++ mMode)
  let fps = [statsDir </> (show ts :: String) <.>  ".total_solving_time"]
  times :: [Maybe Double] <- liftIO  $  forM fps $ readCpuTime
  return . sum . catMaybes $ times


readCpuTime :: (MonadIO m) => FilePath -> m (Maybe Double)
readCpuTime fp = do
  times :: Maybe Double <- liftIO  $ do
             st <- readFile fp
             return $ readMay st
  return times


-- 0.0 perfect  1.0 terrible
calculateParamQuality :: RaceTotals -> Quality
calculateParamQuality RaceTotals{..} =
    if tMinionTimeout == tCount then
        1.0
    else
        1.0 - ((fromIntegral tIsDominated) / (fromIntegral  tCount))


saveQuery :: Query
saveQuery = [str|
  INSERT OR REPLACE INTO ParamQuality(param, paramHash, quality, paramCpuTime)
  Values(?, ?, ?, ?)
  |]


saveQualityToDb :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
                =>  ParamName -> ParamHash -> Quality -> Double ->  m ()
saveQualityToDb paramName paramHash quality cputime = do
  (Method MCommon{mOutputDir} _) <- get
  void $ liftIO $ withConnection (mOutputDir </> "results.db") $ \conn -> do
    execute conn saveQuery (paramName, paramHash, quality, cputime)


checkPreviousQuery :: Query
checkPreviousQuery = "SELECT timestamp FROM Timeouts WHERE paramHash = ?"

checkPrevious :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
              => ParamHash -> m (Maybe TimeStamp)
checkPrevious paramHash =do
  (Method MCommon{mOutputDir} _) <- get
  liftIO $ doesFileExist (mOutputDir </> "results.db") >>= \case
    False -> return Nothing
    True -> do
      withConnection (mOutputDir </> "results.db") $ \conn -> do
        rows :: [Only Int]  <-  query conn checkPreviousQuery (Only paramHash)
        case rows of
          [Only ts] -> return $ Just ts
          _    -> return Nothing


getPointQuery :: Query
getPointQuery = "SELECT quality FROM ParamQuality WHERE paramHash = ?"

getPointQuailty :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
                => ParamHash -> m Quality
getPointQuailty paramHash = do
  (Method MCommon{mOutputDir} _) <- get
  logDebugVerbose2 $line ["before opening db", nn "for hash" paramHash ]
  liftIO $ doesFileExist (mOutputDir </> "results.db") >>= \case
    False -> docError ["db does not exist", pretty $line, "getPointQuailty"]
    True  ->  do
      liftIO $ withConnection (mOutputDir </> "results.db") $ \conn -> do
        rows :: [Only Quality]  <- liftIO $ query conn getPointQuery (Only paramHash)

        case rows of
          [Only qu] -> return $ qu
          xs        -> docError $ "multiple results returned for getPointQuailty"
                                : (pretty $line) : map (pretty . groom) xs


createParamEssence :: (MonadState (Method a) m, MonadIO m, MonadLog m )
                   => m ()
createParamEssence = do
  (Method MCommon{mEssencePath,mVarInfo, mOutputDir} _) <- get
  createParamEssence1 mEssencePath mVarInfo mOutputDir

createParamEssence1 :: (MonadIO m, MonadLog m )
                   => FilePath -> VarInfo -> FilePath -> m ()
createParamEssence1 mEssencePath mVarInfo mOutputDir = do
  let specFp   = (mOutputDir </> "essence_param_find.essence")
  let eprimeFp = (mOutputDir </> "essence_param_find.eprime")
  liftIO $ createDirectoryIfMissing True mOutputDir


  liftIO $ doesFileExist specFp >>= \case
    True  -> return ()
    False -> do
      model     <- liftIO $ ignoreLogs $ readModelFromFile mEssencePath
                     >>= runNameGen . typeCheckModel_StandAlone
                     >>= runNameGen . resolveNames
      paramSpec <- liftIO $ createParamSpecification model mVarInfo
      writeModel PlainEssence (Just $ mOutputDir </> "essence_param_find.essence") paramSpec

  liftIO $ doesFileExist eprimeFp >>= \case
    True  -> return ()
    False -> do
      conjureCompact specFp eprimeFp >>= \case
        True  -> return ()
        False -> error  "Failed to refine the param specification, namely essence_param_find.essence "


createParamSpecification :: (MonadUserError m, MonadFail m) => Model -> VarInfo -> m Model
createParamSpecification model VarInfo{..} = do
  let f = runNameGen . resolveNames  >=> return . inlineLettings >=> core
  ignoreLogs $ f model

  where
  core m = do
    (outStatements, errs) <- runWriterT $ forM (mStatements m) $ \ st -> case st of
      Declaration (FindOrGiven Given nm@(Name te) dom) -> do
        case domainSizeOf dom of
          Nothing -> tell [(nm, dom)] >> return []
          Just (_ :: Expression) -> do
            let k = if te `S.member` givens then Given else Find
            return [Declaration (FindOrGiven k nm dom)]

      Declaration (FindOrGiven Find _  _  ) -> return []
      Declaration     {}                    -> return [st]
      SearchOrder     {}                    -> return []
      Where           xs                    -> return [SuchThat xs]
      Objective       {}                    -> return []
      SuchThat        {}                    -> return []
      SearchHeuristic {}                    -> return []

    if null errs
      then return m { mStatements = concat outStatements }
      else userErr1 $ vcat $ "Given must have a finite domain"
                           : [ pretty nm <> ":" <++> pretty dom
                           | (nm, dom) <- errs
                           ]


conjureCompact :: MonadIO m => FilePath -> FilePath -> m Bool
conjureCompact inn out = do
  liftIO $ withSystemTempDirectory "gen-compact" $ \tmp -> do
    let args = ["-qf", "-ac", inn, "-o", tmp]
    runCommand  "conjure" args Nothing >>= \case
      (ExitFailure _) -> return False
      ExitSuccess     -> do
        -- renameFile (tmp </> "model000001.eprime") out
        -- Not using the above renameFile since it broken when moving across partitions
        -- gen: /tmp/gen-compact546073/model000001.eprime:
        --      renameFile: unsupported operation (Invalid cross-device link)
        copyFile (tmp </> "model000001.eprime") out
        return True


sampleParamFromMinion :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
                      => m (Either SamplingErr Point)
sampleParamFromMinion = do

  (Method MCommon{mOutputDir, mGivensProvider, mParamGenTime} _) <- get

  givens <- provideValues mGivensProvider
  let phash = pointHash givens

  now <- timestamp
  let out        = mOutputDir </> "_param_gen" </> show now
  let paramFp    = (out </> phash) <.> ".param"
  let solutionFp = out </> ("essence_param_find"  ++ "-" ++ phash  <.> ".solution" )

  seed :: Int <- liftIO $ randomRIO (1,2147483647)

  liftIO $ createDirectoryIfMissing True out
  writePoint givens paramFp

  let args = map stringToText [ (mOutputDir </> "essence_param_find.essence")
             , (mOutputDir </> "essence_param_find.eprime")
             , paramFp
             , show mParamGenTime -- Not a bug
             , show mParamGenTime
             , show seed
             ]

  let env = map (second stringToText) [ ("GENERATED_OUTPUT_DIR", out)
            , ("TIMEOUT5_FILE", out </> "timeout_file")
            ]

  cmd <- script_lookup "instances/generate_param.sh"
  void $ liftIO $ runPadded " ⦿ " env cmd args

  worked <- liftIO $ doesFileExist solutionFp
  case worked of
    False -> return $ Left $ ErrFailedToGenerateParam
             (vcat [nn "givens" givens, nn "ts" now, nn "hash" phash ])
    True  -> do
      finds <- readPoint solutionFp
      if finds == Point [] then
        return $ Left $ ErrGeneratedParamEmpty
                     (vcat [ nn "givens" givens, nn "ts" now, nn "hash" phash
                           , nn  "solutionFp" solutionFp])
      else
        return $ Right $ finds `mappend` givens

runSolve  :: (MonadIO m)
          => FilePath -> FilePath -> FilePath -> Point
         -> m (Either SamplingErr (Maybe Point,Double))
runSolve outputDir ess eprime point = do

  let phash   = pointHash point
  let baseName = takeBaseName eprime

  now <- timestamp
  let out        = outputDir </> "_run_solve" </> show now
  let paramFp    = (out </> phash) <.> ".param"
  let solutionFp = out </> (baseName  ++ "-" ++ phash  <.> ".solution" )

  let timeout    = 300 :: Int
  seed :: Int <- liftIO $ randomRIO (1,2147483647)

  liftIO $ createDirectoryIfMissing True out
  writePoint point paramFp

  let args = map stringToText [ ess
             , eprime
             , paramFp
             , show timeout -- used
             , show timeout
             , show seed
             ]

  let env = map (second stringToText) [ ("GENERATED_OUTPUT_DIR", out)
            , ("TIMEOUT5_FILE", out </> "timeout_file")
            ]

  cmd <- script_lookup "instances/run_solve.sh"
  void $ liftIO $ runPadded " ● " env cmd args

  worked <- liftIO $ doesFileExist solutionFp
  liftIO $ print . pretty  $ nn "solutionFp" solutionFp
  timeMay <- readCpuTime (out </> "total.time")

  case (worked, timeMay) of
    (True, Just time)  -> do
      sol <- readPoint solutionFp
      return $ Right (Just sol, time)
    (False, Just time)  -> do
      return $ Right (Nothing, time)
    _ -> return $ Left $ ErrFailedRunSolve
             (vcat [nn "point" point, nn "ts" now, nn "hash" phash,
                    nn  "outdir" out, nn "timeMay" timeMay ])


script_lookup :: MonadIO m => FilePath -> m Text
script_lookup fp = stringToText <$> script_lookup1 fp

script_lookup1 :: MonadIO m => FilePath -> m String
script_lookup1 fp =
  (</> fp)  <$> liftIO (getToolchainDir Nothing)

-- | Run a command with the output padded with leading spaces
-- | n.b  sterr will be redirected to stdin
-- |  parallel -j1 --tagstring may be a better option
runPadded :: String -> [(Text,Text)] -> Text -> [Text] -> IO ()
runPadded ch env cmd args = do
  com <- script_lookup "instances/to_stdout.sh"
  let pad =  " " ++ ch ++ " "
  discardOut <- lookupEnv "NULL_runPadded" >>= \case
    Nothing      -> return False
    Just ""      -> return False
    Just "false" -> return False
    Just _       -> return True


  sh  . print_stdout False . print_stderr False $ do
    mapM_ (\(a,b) ->  setenv a b) env

    let handler _ hout herr = do

          {- Ways that did not work -}
          -- void $ liftIO  $ transferLinesAndCombine hout (printer stdout)
          -- void $ liftIO  $ transferLinesAndCombine herr (printer stderr)

          -- void $ liftIO $ forkIO $ void $
          --      transferFoldHandleLines "" (\_ b-> b) hout (printer stdout)
          -- void $ liftIO $ forkIO $ void $
          --      transferFoldHandleLines "" (\_ b-> b) herr (printer1 stderr)
          if discardOut then
            return ()
          else do
            void $ liftIO  $ transferFoldHandleLines "" (\_ b-> b) hout (printer pad stdout)
            -- There should not be any stderr
            void $ liftIO  $ transferFoldHandleLines "" (\_ b-> b) herr (printer1 stderr)

          return ()

    void $ runHandles (fromString $ textToString com) (cmd:args) [] handler

  where
    printer1 hto lnn = do
      hPutStr hto " ⤬ "
      hPutStrLn hto  (textToString lnn)

    printer pad hto lnn = do
      hPutStr hto $ pad
      hPutStrLn hto  (textToString lnn)
