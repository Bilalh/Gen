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
  , subprocessTotalCpuTime
  ) where

import Conjure.Language
import Conjure.Language.Expression.DomainSizeOf (domainSizeOf)
import Conjure.Language.NameResolution          (resolveNames)
import Conjure.UI.IO
import Data.List                                (foldl1')
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField         ()
import Database.SQLite.Simple.FromRow           ()
import Gen.Helpers.Str
import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.Point
import Gen.IO.Formats
import Gen.IO.Toolchain                         (runCommand)
import Shelly                                   (print_stderr, print_stdout,
                                                 runHandles, setenv,
                                                 transferFoldHandleLines)
import System.Directory                         (copyFile)
import System.Environment                       (lookupEnv)
import System.Exit                              (ExitCode (..))
import System.FilePath                          (takeDirectory)
import System.IO                                (hPutStr, hPutStrLn, readFile,
                                                 stderr, stdout)
import System.IO.Temp                           (withSystemTempDirectory)
import Gen.Helpers.InlineLettings

import qualified Data.Set as S
import Gen.Instance.SamplingError

type TimeStamp = Int
type Quality   = Double



runRace :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
        => ParamFP -> m (Either SamplingErr Quality)
runRace paramFP = do

  ordering <- getModelOrdering
  logDebug2 "runRace ordering:" (map pretty ordering)


  p <- readPoint paramFP
  let paramHash = pointHash p
  let paramName = pointName p

  ts <- doRace paramFP ordering
  (Method MCommon{mOutputDir, mMode} _) <- get
  let resDir = mOutputDir </> ("results" ++ mMode)
  let errorFile = resDir </> ("p-" ++ paramHash ) <.> ".errors"
  erred <- liftIO $ doesFileExist errorFile

  case erred of
    True -> do
      (_, info) <- liftIO $ pairWithContents errorFile
      return $ Left $ ErrRace $ (pretty errorFile <++> pretty info)
    False -> do
      totals <- parseRaceResult (paramHash) ts
      logDebug2 "runRace totals:" [pretty totals]

      let quality = calculateParamQuality totals
      logDebug $ "runRace quality:" <+> pretty quality

      timeTaken <- readParamRaceCpuTime ts
      saveQualityToDb paramName paramHash quality timeTaken

      -- FIXME store prev_timestamp if doing a cpu limit
      return $ Right quality


getModelOrdering :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
        => m [ FilePath ]
getModelOrdering = do
  (Method MCommon{mOutputDir, mCompactFirst, mModelsDir} _) <- get
  let dbPath  =  mOutputDir </> "results.db"
  liftIO $ doesFileExist dbPath >>= \case
    False -> do
      case mCompactFirst of
        Nothing -> return []
        Just xs -> return xs
    True  -> do
      conn <- liftIO $ open dbPath
      eprimes :: [[String]] <- query_ conn ("SELECT eprime FROM EprimeOrdering")
      return [ mModelsDir </> row `at` 0 | row <- eprimes  ]


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

  cmd <- wrappers "run.sh"
  liftIO $ runPadded "⌇" env (stringToText cmd) args
  return now


raceResultsQuery :: Query
raceResultsQuery = [str|
  SELECT  1, MinionTimeout, MinionSatisfiable,MinionSolutionsFound, IsOptimum, isDominated
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

  cmd <- wrappers "run_gather.sh"
  liftIO $ runPadded "❮" env (stringToText cmd) args

  conn <- liftIO $ open (mOutputDir </> "results.db")
  rows :: [RaceTotals] <- liftIO $ query conn raceResultsQuery (Only paramHash)
  let total = flip foldl1' rows (\(RaceTotals a1 b1 c1 d1 e1 f1)
                                  (RaceTotals a2 b2 c2 d2 e2 f2)
            -> (RaceTotals (a1+a2) (b1+b2) (c1+c2) (d1+d2) (e1+e2) (f1+f2)) )

  logDebugVerbose $ "Totals for " <+> pretty paramHash <+> pretty ts
  mapM_ (logDebugVerbose . pretty . groom) rows

  return total


data RaceTotals = RaceTotals
    { tCount                :: Int
    , tMinionTimeout        :: Int
    , tMinionSatisfiable    :: Int
    , tMinionSolutionsFound :: Int
    , tIsOptimum            :: Int
    , tIsDominated          :: Int
    } deriving (Eq, Show, Data, Typeable, Generic)
instance Pretty RaceTotals where pretty = pretty . groom

instance FromRow RaceTotals where
    fromRow = RaceTotals <$> field <*> field <*> field <*> field <*> field <*> field


subprocessTotalCpuTime :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
                       => m Double
subprocessTotalCpuTime = do
  (Method MCommon{mOutputDir, mMode} _) <- get
  let statsDir = mOutputDir </> ("stats_" ++ mMode)
  fps <- liftIO $ allFilesWithSuffix ".total_solving_time" statsDir
  times :: [Maybe Double] <- liftIO  $  forM fps $ \fp -> do
             st <- readFile fp
             return $ readMay st
  return . sum . catMaybes $ times


readParamRaceCpuTime :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
                     => TimeStamp -> m Double
readParamRaceCpuTime ts = do
  (Method MCommon{mOutputDir, mMode} _) <- get
  let statsDir = mOutputDir </> ("stats_" ++ mMode)
  let fps = [statsDir </> (show ts :: String) <.>  ".total_solving_time"]
  times :: [Maybe Double] <- liftIO  $  forM fps $ \fp -> do
             st <- readFile fp
             return $ readMay st
  return . sum . catMaybes $ times


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
  conn <- liftIO $ open (mOutputDir </> "results.db")
  liftIO $ execute conn saveQuery (paramName, paramHash, quality, cputime)
  return ()


checkPreviousQuery :: Query
checkPreviousQuery = "SELECT timestamp FROM Timeouts WHERE paramHash = ?"

checkPrevious :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
              => ParamHash -> m (Maybe TimeStamp)
checkPrevious paramHash =do
  (Method MCommon{mOutputDir} _) <- get
  liftIO $ doesFileExist (mOutputDir </> "results.db") >>= \case
    False -> return Nothing
    True -> do
      conn <- liftIO $ open (mOutputDir </> "results.db")
      rows :: [Only Int]  <- liftIO $ query conn checkPreviousQuery (Only paramHash)

      case rows of
        [Only ts] -> return $ Just ts
        _    -> return Nothing


createParamEssence :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
                   => m ()
createParamEssence = do
  logDebug "createParamEssence"
  (Method MCommon{mEssencePath,mVarInfo, mOutputDir} _) <- get
  let specFp   = (mOutputDir </> "essence_param_find.essence")
  let eprimeFp = (mOutputDir </> "essence_param_find.eprime")
  liftIO $ createDirectoryIfMissing True mOutputDir


  liftIO $ doesFileExist specFp >>= \case
    True  -> return ()
    False -> do
      model     <- liftIO $ readModelFromFile mEssencePath
      paramSpec <- liftIO $ createParamSpecification model mVarInfo
      writeModel PlainEssence (Just $ mOutputDir </> "essence_param_find.essence") paramSpec

  liftIO $ doesFileExist eprimeFp >>= \case
    True  -> return ()
    False -> do
      conjureCompact specFp eprimeFp >>= \case
        True  -> return ()
        False -> userErr1 "Failed to refine the param specification, namely essence_param_find.essence "


createParamSpecification :: (MonadUserError m, MonadFail m) => Model -> VarInfo -> m Model
createParamSpecification model VarInfo{..} = do
  let f = runNameGen . resolveNames  >=> return . inlineLettings >=> core
  ignoreLogs $ f model

  where
  core m = do
    (outStatements, errs) <- runWriterT $ forM (mStatements m) $ \ st -> case st of
      Declaration (FindOrGiven Given nm@(Name te) dom) ->
        case domainSizeOf dom of
          Nothing -> tell [(nm, dom)] >> return []
          Just (_ :: Expression) -> do
            let k = if te `S.member` givens then Given else Find
            return [Declaration (FindOrGiven k nm dom)]

      Declaration (FindOrGiven Find _  _  ) -> return []
      Declaration {}                        -> return [st]
      SearchOrder {}                        -> return []
      Where       xs                        -> return [SuchThat xs]
      Objective   {}                        -> return []
      SuchThat    {}                        -> return []

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

  (Method MCommon{mOutputDir, mGivensProvider} _) <- get

  givens <- provideValues mGivensProvider
  let phash = pointHash givens

  now <- timestamp
  let out        = mOutputDir </> "_param_gen" </> show now
  let paramFp    = (out </> phash) <.> ".param"
  let solutionFp = out </> ("essence_param_find"  ++ "-" ++ phash  <.> ".solution" )

  let timeout    = 300 :: Int
  seed :: Int <- liftIO $ randomRIO (1,2147483647)

  liftIO $ createDirectoryIfMissing True out
  writePoint givens paramFp

  let args = map stringToText [ (mOutputDir </> "essence_param_find.essence")
             , (mOutputDir </> "essence_param_find.eprime")
             , paramFp
             , show timeout
             , show timeout
             , show seed
             ]

  let env = map (second stringToText) [ ("GENERATED_OUTPUT_DIR", out)
            , ("TIMEOUT5_FILE", out </> "timeout_file")
            ]

  cmd <- wrappers "create_param_from_essence.sh"
  res <- liftIO $ runPadded " ⦿ " env (stringToText cmd) args

  worked <- liftIO $ doesFileExist solutionFp
  case worked of
    False -> return $ Left $ ErrFailedToGenerateParam
             (vcat [nn "givens" givens, nn "ts" now, nn "hash" phash ])
    True  -> do
      finds <- readPoint solutionFp
      return $ Right $ finds `mappend` givens



wrappers :: MonadIO m => FilePath -> m FilePath
wrappers fp = do
  liftIO $ lookupEnv ("PARAM_GEN_SCRIPTS" :: String) >>= \case
            Nothing -> liftIO $ error "No PARAM_GEN_SCRIPTS variable"
            Just p -> do
                return $ p </> "wrappers" </> fp


-- | Run a command with the output padded with leading spaces
-- | n.b  sterr will be redirected to stdin
runPadded :: String -> [(Text,Text)] -> Text -> [Text] -> IO ()
runPadded ch env cmd args = do
  com <- wrappers "to_stdout.sh"
  let pad =  " " ++ ch ++ " "
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

          void $ liftIO  $ transferFoldHandleLines "" (\_ b-> b) hout (printer pad stdout)
          -- There should not be any stderr
          void $ liftIO  $ transferFoldHandleLines "" (\_ b-> b) herr (printer1 stderr)

          return ()

    void $ runHandles (fromString com) (cmd:args) [] handler

  where
    printer1 hto lnn = do
      hPutStr hto " ⤬ "
      hPutStrLn hto  (textToString lnn)

    printer pad hto lnn = do
      hPutStr hto $ pad
      hPutStrLn hto  (textToString lnn)
