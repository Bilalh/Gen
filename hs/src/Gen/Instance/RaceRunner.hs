{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
module Gen.Instance.RaceRunner(
    runRace
  , doRace
  , parseRaceResult
  , createParamEssence
  , sampleParamFromMinion
  , RaceTotals(..)
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
import Gen.Instance.Value
import Gen.IO.Formats
import Gen.IO.Toolchain                         (runCommand)
import Shelly                                   (print_stderr, print_stdout,
                                                 runHandles, setenv,
                                                 transferFoldHandleLines)
import System.Directory                         (renameFile)
import System.Environment                       (lookupEnv)
import System.Exit                              (ExitCode (..))
import System.FilePath                          (takeBaseName, takeDirectory)
import System.IO                                (hPutStr, hPutStrLn, readFile,
                                                 stderr, stdout)
import System.IO.Temp                           (withSystemTempDirectory)

import qualified Data.Set as S

type TimeStamp = Int
type Quality   = Double

mMode = "df"
mDataPoints = $notDone


runRace :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
        => ParamFP -> m Quality
runRace paramFP = do

  ordering <- getModelOrdering
  logDebug2 "runRace ordering:" (map pretty ordering)

  let paramHash = takeBaseName paramFP
  let paramName = takeBaseName paramFP

  ts <- doRace paramFP

  totals <- parseRaceResult (paramHash) ts
  logDebug2 "runRace totals:" [pretty totals]

  let quality = calculateParamQuality totals
  logDebug $ "runRace quality:" <+> pretty quality

  timeTaken <- readParamRaceCpuTime ts
  saveQualityToDb paramName paramHash quality timeTaken

  -- FIXME store prev_timestamp if doing a cpu limit
  return quality


getModelOrdering :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
        => m [ FilePath ]
getModelOrdering = do
  (Method MCommon{mEssencePath, mOutputDir} _) <- get
  let dbPath  =  mOutputDir </> "results.db"
  liftIO $ doesFileExist dbPath >>= \case
    False -> return []
    True  -> do
      conn <- liftIO $ open dbPath
      eprimes :: [[String]] <- query_ conn ("SELECT eprime FROM EprimeOrdering")
      let mModelsDir = takeDirectory mEssencePath </> takeBaseName mEssencePath ++ mMode
      let paths = [ mModelsDir </> row `at` 0 | row <- eprimes  ]

      lookupEnv ("LIMIT_MODELS" :: String) >>= \case
         Nothing   -> return paths
         Just sInt -> do
           case readMay sInt of
             Nothing  -> docError ["LIMIT_MODELS specifed but is not a int:"
                                  , pretty sInt]
             (Just i) -> return $ take i paths


doRace :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
        => ParamFP -> m TimeStamp
doRace paramFP = do
  (Method MCommon{mEssencePath, mOutputDir, mModelTimeout} _) <- get
  now <- timestamp


  let args = map stringToText
             [ show now
             , paramFP
             , show mModelTimeout
             , takeDirectory mEssencePath
             ]

  let env = [ ("NUM_JOBS", "2")
            , ("USE_MODE",stringToText mMode)
            , ("OUT_BASE_DIR", stringToText mOutputDir)
            , ("LIMIT_MODELS", "3")  -- Only race the first 3 models
            ]

  cmd <- wrappers "run.sh"
  -- res <- runCommand' (Just env) cmd args Nothing
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
  (Method MCommon{mEssencePath, mOutputDir, mModelTimeout} _) <- get

  let args = map stringToText [ paramHash
             , takeDirectory mEssencePath
             ]

  let env = map (second stringToText) [ ("USE_DATE", show ts)
            , ("TOTAL_TIMEOUT", show mModelTimeout)
            , ("USE_MODE", mMode)
            , ("OUT_BASE_DIR", mOutputDir)
            ]

  cmd <- wrappers "run_gather.sh"
  -- res <- runCommand' (Just env) cmd args Nothing
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


readParamRaceCpuTime :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
                     => TimeStamp -> m Double
readParamRaceCpuTime ts = do
  (Method MCommon{mOutputDir} _) <- get
  let statsDir = mOutputDir </> ("stats_" ++ mMode)
  -- fps   <- liftIO  $ allFilesWithSuffix ".total_solving_time" statsDir
  let fps = [statsDir </> (show ts :: String) <.>  ".total_solving_time"]
  times :: [Maybe Double] <- liftIO  $  forM fps $ \fp -> do
             st <- readFile fp
             return $ readMay st
  return . sum . catMaybes $ times


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
  let f = runNameGen . resolveNames  >=> core
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
        renameFile (tmp </> "model000001.eprime") out
        return True


sampleParamFromMinion :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
                      => m Point
sampleParamFromMinion = do

  let point = Point []
  let phash = pointHash point

  (Method MCommon{mOutputDir} _) <- get
  now <- timestamp
  let out        = mOutputDir </> "_param_gen" </> show now
  let paramFp    = (out </> phash) <.> ".param"
  let solutionFp = out </> ("essence_param_find"  ++ "-" ++ phash  <.> ".solution" )

  let timeout    = 300 :: Int
  let seed       = 4   :: Int

  liftIO $ createDirectoryIfMissing True out
  writeParam point paramFp

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

  (Point ps) <- readParam solutionFp
  -- FIXME append the givens

  return $ Point ps


writeParam :: MonadIO m => Point  -> FilePath -> m ()
writeParam (Point ps) fp = do
  let sts = [ Declaration (Letting (label) (Constant con))
            |  (label,con) <- ps ]
  let m :: Model = def{mStatements=sts}
  liftIO $ writeModel PlainEssence (Just fp) m


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
