{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
module Gen.IO.SmacProcess where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Instantiate (instantiateDomain)
import Gen.Imports                  hiding (group)
import Gen.Instance.Data
import Gen.Instance.Method
import Gen.Instance.Point
import Gen.Instance.RaceRunner      (RaceTotals (..), createParamEssence,
                                     getPointQuailty, initDB, parseRaceResult,
                                     raceCpuTime)
import Gen.Instance.Results.Results
import Gen.Instance.SamplingError
import Conjure.Process.Enumerate(enumerateDomain)
import Gen.Instance.UI
import Gen.IO.FindCompact
import Gen.IO.Formats               (allGivensOfEssence, getFullPath, readFromJSON,
                                     readFromJSONMay, timestamp, writeToJSON)
import System.CPUTime               (getCPUTime)
import System.Directory             (makeAbsolute)
import System.Environment           (lookupEnv)
import System.FilePath.Posix        (replaceFileName, takeBaseName)
import System.IO                    (hPutStrLn, stderr)
import System.Random                (mkStdGen, setStdGen)
import Text.Printf

import qualified Data.Aeson                      as A
import qualified Data.Text                       as T
import qualified Data.Vector                     as V
import qualified Gen.Instance.Results.SettingsIn as IN


smacProcess :: (MonadIO m, MonadLog m)
            => FilePath -> String -> String -> Double -> Int -> Int -> [String]
            -> m ()
smacProcess s_output_directory _s_eprime _s_instance_specific
  s_cutoff_time _s_cutoff_length s_seed s_param_arr = do
  liftIO $ setStdGen (mkStdGen s_seed)
  startOurCPU <- liftIO $  getCPUTime
  rTimestampStart <- timestamp

  vs <- liftIO $ V.toList <$> decodeCSV (s_output_directory </> "settings.csv")
  let x@IN.CSV_IN{..} = headNote "setting.csv should have one row" vs

  let modelTime :: Int = min 1 $ truncate $ s_cutoff_time
                           / ((fromIntegral num_models) :: Double)

  out $line $ show . vcat $  [ nn "cutoff_time" s_cutoff_time
                             , nn "per_model_time_given" per_model_time_given
                             , nn "race_time_given" race_time_given
                             , nn "# models" num_models
                             , nn "Calculated modelTime" modelTime
                             ]

  out $line $ groom x
  -- out $line $ groom s_param_arr


  essenceA <- liftIO $ getFullPath essence
  out $line $ essenceA

  givens <- liftIO $ allGivensOfEssence  essenceA
  out $line $ show $ map pretty givens

  point <- parseParamArray s_param_arr givens
  out $line $ show $ pretty $ point

  prevState <- loadState x point modelTime
  prevMeta  <- loadRunMetaData

  (Method _ thisSmac) <- s_runMethod prevState
  out $line $ groom thisSmac
  let (thisQuality, thisTotals, raceTime) = fromJustNote "Needa a result" $ sResult thisSmac

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

  let runtime = rOurCPUTime  + raceTime
  outputResult resultType runtime 0  (truncate smacQuality) s_seed


parseParamArray :: MonadIO m => [String] -> [(Text,Domain () Expression)]
                -> m  Point
parseParamArray arr givens = do
  let tuples = process arr
  -- out $line $  groom tuples
  -- Strip the prefix off the encoded
  let grouped = map (\(name,dom) -> (name,dom,
                     sort [ (fromJustNote $line $ T.stripPrefix name t,v)
                          | (t,v) <- tuples, name `T.isPrefixOf` t ]
                    ) ) givens
  out $line $ groom grouped
  vs <- flip evalStateT [] $ forM grouped $ \g -> do
                             g'@(name,dom,_) <- instantiatePrev g
                             out $line $ show (nn ("Working on" <+> pretty name) dom)
                             res <- parseSmacValues g'
                             out $line $ show (nn ("Result on" <+> pretty name) res)
                             return res

  return $ Point vs


  where
  process :: [String] -> [(Text,Integer)]
  process []       = []
  process [x]      = error $ "single element" ++ show x
  process (x:y:zs) = (T.pack $ tail x, fromJustNote "must be an Int" $ readMay ((init . tail) y))
                   : process zs

  instantiatePrev (name, d1, vs) = do
     prev <- get
     d2 <- liftIO $ instantiateDomain prev d1
     return (name, d2, vs)

  -- Parse the encoded values back to essence
  parseSmacValues (name,DomainInt{},[(_,i)]) = do
   modify $ \st -> (Name name, Constant $ ConstantInt i) : st
   return (Name name, ConstantInt i)

  parseSmacValues (name,
    (DomainFunction _ (FunctionAttr SizeAttr_None PartialityAttr_Total JectivityAttr_None)
                    (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt upper)])
                     DomainInt{}) , vs) = do
    let tuples = genericTake upper [ (parse "%FT%" t  , ConstantInt v) | (t,v) <- vs ]
    return $ (Name name, ConstantAbstract $ AbsLitFunction tuples)

  -- Decoding would work for any function (total) tuple -> int
  parseSmacValues (name,
    (DomainFunction _ (FunctionAttr SizeAttr_None PartialityAttr_Total JectivityAttr_None)
      tu@(DomainTuple [DomainInt [RangeBounded (ConstantInt 1) (ConstantInt size1)]
                      ,DomainInt [RangeBounded (ConstantInt 1) (ConstantInt size2)]] )
       DomainInt{}) , vs) = do
    let size   = size1 * size2
    let vals   = genericTake size
                 [ (ConstantInt v) | (t,v) <- vs, "%FT_TV%" `T.isInfixOf` t ]
    allTuples <- liftIO $ enumerateDomain tu
    let tuples = zip allTuples vals
    return $ (Name name, ConstantAbstract $ AbsLitFunction tuples)


  parseSmacValues (name,dom,vs) = do
    lineError $line ["unhandled", nn "name" name, nn "dom" dom, nn "vs" (groom vs) ]

  parse kind t =  fromJustNote ("Failed parsing " ++ show t) $ ConstantInt
              <$> (T.stripPrefix kind t >>= return . T.unpack >>= readMay)


-- | like run method but with some parts omitted
s_runMethod :: ( MonadIO m, MonadLog m)
            => (Bool, Method Smac) ->  m (Method Smac)
s_runMethod (initValues, state) = do
  flip execStateT state $ do
    when initValues $ createParamEssence >> initDB >> doSaveEprimes True
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
  , rRacesCPUTime                  = rRacesCPUTime rd
  , rParamGenCPUTime               = rParamGenCPUTime rd
  , rSubCPUTime                    = rSubCPUTime prev + rSubCPUTime rd
  , rOurCPUTime                    = rOurCPUTime prev + cpuTime
  , rIterationsDone                = rIterationsDone prev + rIterationsDone rd
  , rIterationsDoneIncludingFailed = rIterationsDoneIncludingFailed prev + rIterationsDoneIncludingFailed rd
  }

-- | Load the state from disk if it exists otherwise init it.
loadState :: MonadIO m => IN.CSV_IN -> Point -> Int -> m (Bool, Method Smac)
loadState dat point given = liftIO $ doesFileExist "state.json" >>= \case
  False -> (\x -> (True, x))  <$> initState dat point given
  True  -> do
    (Method common Smac{}) <- readFromJSON "state.json"
    return $ (False, Method common{mModelTimeout=given} (smacInit point))


initState :: MonadIO m => IN.CSV_IN -> Point -> Int -> m (Method Smac)
initState IN.CSV_IN{..} point  mModelTimeout = do
  essenceA <- liftIO $ getFullPath essence
  let info_path   = replaceFileName essenceA "info.json"
      models_path = replaceFileName essenceA (takeBaseName essenceA ++ "_" ++ mode)

  compactFirst <- lookupCompact models_path essenceA
  cores        <- liftIO $ fromJustNote "CORES must be set" <$> lookupEnv "CORES"

  i <- liftIO $ readFromJSON info_path
  p <- ignoreLogs $ makeProvider essenceA i
  outDir <- liftIO $ makeAbsolute "."

  let common            = MCommon{
        mEssencePath    = essenceA
      , mOutputDir      = outDir
      , mModelTimeout
      , mVarInfo        = i
      , mPreGenerate    = Nothing
      , mIterations     = 1
      , mMode           = mode
      , mModelsDir      = models_path
      , mGivensProvider = p
      , mPoints         = []
      , mCores          = fromJustNote "CORES must be an int" $ readMay cores
      , mCompactName    = compactFirst
      , mSubCpu         = 0
      , mPointsGiven    = Nothing
      , mParamGenTime   = 300 -- Unused
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
--FIXME need to use validate solution?

data Smac = Smac{
      sPoint   :: Point
    , sResult  :: Maybe (Quality, RaceTotals, Double)
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
      Left (ErrDuplicatedPoint _ ts) -> do
        let h = pointHash sPoint
        totals  <- parseRaceResult h ts
        quality <- getPointQuailty h
        modify $ \(Method common st) ->
          (Method common st{sResult = Just (quality, totals, 0) })
        return $ Right ()

      Left err -> return $ Left err

      Right (quality, totals, ts)  -> do
        storeDataPoint sPoint
        raceTime <- fromJustNote "Must have cpuTime" <$> raceCpuTime ts
        modify $ \(Method common st) ->
            (Method common st{sResult = Just (quality, totals, raceTime)})
        return $ Right ()
