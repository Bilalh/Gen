{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Gen.IO.SmacProcess where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Data.Csv                         (DefaultOrdered, FromNamedRecord,
                                         ToNamedRecord, decodeByName,
                                         encodeDefaultOrderedByName)
import Data.List                        (break)
import Data.Map                         (Map)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField ()
import Database.SQLite.Simple.FromRow   ()
import Gen.Helpers.Str
import Gen.Imports                      hiding (group)
import Gen.Instance.Data
import Gen.Instance.Method
import Gen.Instance.Point
import Gen.Instance.RaceRunner          (initDB, getPointQuailty,RaceTotals(..))
import Gen.Instance.Results.Results
import Gen.Instance.UI
import Gen.Instance.UI
import Gen.IO.FindCompact
import Gen.IO.Formats                   (allGivensOfEssence, getFullPath,
                                         readFromJSON, readFromJSONMay, timestamp,writeToJSON)
import System.CPUTime                   (getCPUTime)
import System.Directory                 (getHomeDirectory)
import System.Directory                 (makeAbsolute)
import System.Environment               (lookupEnv)
import System.Exit                      (ExitCode (ExitSuccess))
import System.FilePath                  (takeDirectory)
import System.FilePath.Posix            (replaceFileName, takeBaseName)
import System.IO                        (hPutStrLn, stderr)
import System.Random                    (mkStdGen, setStdGen)
import Text.Printf

import qualified Data.Map                        as M
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import qualified Data.Vector                     as V
import qualified Gen.Instance.Results.SettingsIn as IN
import qualified Data.Aeson as A


smacProcess :: (MonadIO m, MonadLog m) => forall t t1 t2 t3 .
               FilePath -> t -> t1 -> t2 -> t3 -> Int -> [String] -> m ()
smacProcess s_output_directory s_eprime s_instance_specific
  s_cutoff_time s_cutoff_length s_seed s_param_arr = do
  liftIO $ setStdGen (mkStdGen s_seed)
  startOurCPU <- liftIO $  getCPUTime
  rTimestampStart <- timestamp

  vs <- liftIO $ V.toList <$> decodeCSV (s_output_directory </> "settings.csv")
  let x@IN.CSV_IN{..} = headNote "setting.csv should have one row" vs

  out $line "smacProcess"
  out $line $ groom s_param_arr
  out $line $ groom x


  essenceA <- liftIO $ getFullPath essence
  out $line $ essenceA

  givens <- liftIO $ allGivensOfEssence  essenceA
  out $line $ show $ map pretty givens

  point <- parseParamArray s_param_arr givens
  out $line $ show $ pretty $ point

  prevState <- loadState x point
  prevMeta  <- loadRunMetaData

  (Method _ thisSmac) <- s_runMethod prevState
  out $line $ groom thisSmac
  let (thisQuality, thisTotals) = fromJustNote "Must have a result" $ sResult thisSmac

  endOurCPU <- liftIO $ getCPUTime
  let rOurCPUTime = fromIntegral (endOurCPU - startOurCPU) / ((10 :: Double) ^ (12 :: Int))
  rTimestampEnd <- timestamp

  thisMeta <- fromJustNote "RunMetaData must be created" <$> loadRunMetaData
  out ($line ++ " this RunMetaData") $ groom thisMeta

  let newMeta =  combineMeta prevMeta thisMeta rTimestampStart rTimestampEnd rOurCPUTime
  writeRunMetaData newMeta
  out ($line ++ " combined RunMetaData") $ groom newMeta

  let smacQuality = thisQuality * 100
  let resultType = if tCount thisTotals == tMinionTimeout thisTotals then
                       "TIMEOUT"
                   else if tMinionSatisfiable thisTotals > 0  then
                       "SAT"
                   else
                       "UNSAT"

  let runtime = rCPUTime thisMeta
  outputResult resultType runtime 0  (truncate smacQuality) s_seed


parseParamArray :: MonadIO m => [String] -> [(Text,Domain () Expression)]
                -> m  Point
parseParamArray arr givens = do
  let tuples = process arr
  out $line $  groom tuples
  -- Strip the prefix off the encoded
  let grouped = map (\(name,dom) -> (name,dom,
                                     [ (T.stripPrefix name t,v)  | (t,v) <- tuples, name `T.isPrefixOf` t ]
                                    ) ) givens
  out $line $ groom grouped
  let vs = sort $ map parseSmacValues grouped

  return $ Point vs


  where
  process :: [String] -> [(Text,Integer)]
  process []       = []
  process [x]      = error $ "single element" ++ show x
  process (x:y:zs) = (T.pack $ tail x, fromJustNote "must be an Int" $ readMay ((init . tail) y))
                   : process zs

  -- Parse the encoded values back to essence
  parseSmacValues (name,DomainInt{},[(_,i)]) = (Name name, ConstantInt i)



-- | like run method but with some parts omitted
s_runMethod :: ( MonadIO m, MonadLog m)
            => (Bool, Method Smac) ->  m (Method Smac)
s_runMethod (initValues, state) = do
  flip execStateT state $ do
    when initValues $ initDB >> doSaveEprimes True
    handleWDEG >> run

-- | Load RunMetadata if it exists
loadRunMetaData :: MonadIO m => m (Maybe RunMetadata)
loadRunMetaData = readFromJSONMay "metadata.json"

writeRunMetaData :: MonadIO m => RunMetadata -> m ()
writeRunMetaData rd = liftIO $ writeToJSON "metadata.json" rd

combineMeta :: Maybe RunMetadata -> RunMetadata -> Int -> Int -> Double
            -> RunMetadata
combineMeta Nothing rd  tsStart tsEnd cpuTime =
  rd{
    rTimestampStart = min (rTimestampStart rd) tsStart
  , rTimestampEnd   = max (rTimestampEnd rd) tsEnd
  , rOurCPUTime     = cpuTime
  , rCPUTime        = rCPUTime rd + (cpuTime -  rOurCPUTime rd)
  }

combineMeta (Just prev) rd tsStart tsEnd cpuTime =
  RunMetadata
  { rTimestampStart                = min (rTimestampStart prev) tsStart
  , rTimestampEnd                  = max (rTimestampEnd rd) tsEnd
  , rRealTime                      = rRealTime prev + rRealTime rd
  , rCPUTime                       = rCPUTime prev + rCPUTime rd + (cpuTime -  rOurCPUTime rd)
  , rRacesCPUTime                  = rRacesCPUTime prev + rRacesCPUTime rd
  , rParamGenCPUTime               = rParamGenCPUTime prev + rParamGenCPUTime rd
  , rSubCPUTime                    = rSubCPUTime prev + rSubCPUTime rd
  , rOurCPUTime                    = rOurCPUTime prev + cpuTime
  , rIterationsDone                = rIterationsDone prev + rIterationsDone rd
  , rIterationsDoneIncludingFailed = rIterationsDoneIncludingFailed prev + rIterationsDoneIncludingFailed rd
  }

-- | Load the state from disk if it exists otherwise init it.
loadState :: MonadIO m => IN.CSV_IN -> Point -> m (Bool, Method Smac)
loadState dat point = liftIO $ doesFileExist "state.json" >>= \case
  False -> (\x -> (True, x))  <$> initState dat point
  True  -> do
    (Method common Smac{}) <- readFromJSON "state.json"
    return $ (False, Method common (smacInit point))

initState :: MonadIO m => IN.CSV_IN -> Point -> m (Method Smac)
initState IN.CSV_IN{..} point = do
  essenceA <- liftIO $ getFullPath essence
  let info_path   = replaceFileName essenceA "info.json"
      models_path = replaceFileName essenceA (takeBaseName essenceA ++ "_" ++ mode)

  compactFirst <- lookupCompact models_path essenceA
  cores        <- liftIO $ fromJustNote "CORES must be set" <$> lookupEnv "CORES"

  i <- liftIO $ readFromJSON info_path
  p <- ignoreLogs $ makeProvider essenceA i
  outDir <- liftIO $ makeAbsolute "."

  let common          = MCommon{
        mEssencePath    = essenceA
      , mOutputDir      = outDir
      , mModelTimeout   = per_model_time_given
      , mVarInfo        = i
      , mPreGenerate    = Nothing
      , mIterations     = 1
      , mMode           = mode
      , mModelsDir      = models_path
      , mGivensProvider = p
      , mPoints         = []
      , mCores          = 4
      , mCompactName    = compactFirst
      , mSubCpu         = 0
      , mPointsGiven    = Nothing
      , mParamGenTime   = 300
      }

  return $ Method common (smacInit point)

-- | This needs to be the last line
outputResult :: MonadIO m => String -> Double -> Int -> Int -> Int -> m ()
outputResult result_kind runtime runlength quality seed = do
  let s = printf "Final Result for ParamILS: %s, %f, %d, %d, %d\nresult_kind runtime runlength quality seed\n"
          result_kind runtime runlength quality seed
  out $line s
  liftIO $ printf "Final Result for ParamILS: %s, %f, %d, %d, %d\n"
          result_kind runtime runlength quality seed

-- | Allows us to see the output in the logs
out :: MonadIO m => String -> String -> m ()
out l s = liftIO $ hPutStrLn stderr $ unlines $ ('»' : ' ' : l)
        : [ '»' : ' ' : xs | xs <- lines s ]

{- Use the parsed point to run 1 race  -}

--FIXME handle duplicates, it goes into an inf loop at the moment
--FIXME need to use validate solution

data Smac = Smac{
      sPoint   :: Point
    , sResult  :: Maybe (Quality, RaceTotals)
    }
  deriving (Eq, Show, Data, Typeable, Generic)

smacInit :: Point -> Smac
smacInit p = Smac{sPoint=p, sResult=Nothing}

instance A.FromJSON Smac
instance A.ToJSON Smac

instance Sampling Smac where
  doIteration = do
    (Method _ Smac{..}) <- get
    runParamAndStoreQuality sPoint >>= \case
      Left err -> return $ Left err
      Right res  -> do
        storeDataPoint sPoint
        modify $ \(Method common st) -> (Method common st{sResult = Just res})
        return $ Right ()
