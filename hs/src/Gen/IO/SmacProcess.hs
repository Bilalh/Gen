{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
module Gen.IO.SmacProcess where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Instantiate (instantiateDomain)
import Conjure.Language.Name
import Conjure.Process.Enumerate    (enumerateDomain)
import Conjure.Process.Enumerate    (EnumerateDomain)
import Conjure.UI.IO                (readModelFromFile)
import Conjure.UI.ValidateSolution  (validateSolution)
import Gen.Imports                  hiding (group)
import Gen.Instance.Data
import Gen.Instance.Method
import Gen.Instance.Point
import Gen.Instance.RaceRunner      (RaceTotals (..), createParamEssence,
                                     getPointQuailty, initDB, parseRaceResult,
                                     raceCpuTime)
import Gen.Instance.Results.Results
import Gen.Instance.SamplingError
import Gen.Instance.UI
import Gen.IO.FindCompact
import Gen.IO.Formats               (allGivensOfEssence, getFullPath, readFromJSON,
                                     readFromJSONMay, timestamp, writeToJSON)
import Gen.IO.SmacValues
import System.CPUTime               (getCPUTime)
import System.Directory             (makeAbsolute)
import System.Environment           (lookupEnv)
import System.FilePath.Posix        (replaceFileName, takeBaseName)
import System.IO                    (hPutStrLn, stderr)
import Text.Printf

import qualified Data.Aeson                      as A
import qualified Data.Set                        as S
import qualified Data.Text                       as T
import qualified Data.Vector                     as V
import qualified Gen.Instance.Results.SettingsIn as IN


smacProcess :: (MonadIO m, MonadLog m, EnumerateDomain m)
            => FilePath -> String -> String -> Double -> Int -> Int -> [String]
            -> m ()
smacProcess s_output_directory _s_eprime _s_instance_specific
  s_cutoff_time _s_cutoff_length _s_seed s_param_arr = do
  startOurCPU <- liftIO $  getCPUTime
  rTimestampStart <- timestamp

  vs <- liftIO $ V.toList <$> decodeCSV (s_output_directory </> "settings.csv")
  let x@IN.CSV_IN{..} = headNote "setting.csv should have one row" vs

  let modelTime :: Int = max 1 $ truncate $ s_cutoff_time
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

  (needsInit, preInitState@(Method comm _)) <- loadState x point modelTime
  prevState <- if needsInit then
    s_runInit preInitState
  else
      return preInitState

  prevMeta <- loadRunMetaData

  vaildateSpec <- liftIO $ readModelFromFile
                    (s_output_directory </> "essence_param_find.essence")
  isValid <- validatePoint vaildateSpec (mGivensProvider comm) point

  if isValid then do
    out $line $ "Point valid"
    runParam startOurCPU rTimestampStart _s_seed prevState prevMeta
  else do
    out $line $ "Point invaild"
    endOurCPU <- liftIO $ getCPUTime
    let ourCPUTime = fromIntegral (endOurCPU - startOurCPU) / ((10 :: Double) ^ (12 :: Int))
    rTimestampEnd <- timestamp
    let ourTime = (rTimestampEnd - rTimestampStart)

    thisMetaMay <- loadRunMetaData
    let newMeta = case thisMetaMay of
          Nothing -> RunMetadata{ rTimestampStart
          , rTimestampEnd
          , rRealTime                      = ourTime
          , rCPUTime                       = ourCPUTime
          , rRacesCPUTime                  = 0
          , rParamGenCPUTime               = 0
          , rSubCPUTime                    = 0
          , rOurCPUTime                    = ourCPUTime
          , rIterationsDone                = 0
          , rIterationsDoneIncludingFailed = 1
          }

          (Just thisMeta) -> thisMeta{ rTimestampEnd
          , rRealTime   =  rRealTime thisMeta   + ourTime
          , rOurCPUTime =  rOurCPUTime thisMeta + ourCPUTime
          , rCPUTime    =  rCPUTime thisMeta    + ourCPUTime
          , rIterationsDoneIncludingFailed =
            rIterationsDoneIncludingFailed thisMeta + 1}

    out ($line ++ " prev RunMetaData") $ groom thisMetaMay
    out ($line ++ " new RunMetaData") $ groom newMeta

    writeRunMetaData newMeta


    let smacQuality = 500
    let resultType  = "SAT"
    outputResult resultType ourCPUTime 0 smacQuality _s_seed


-- | Validate the point with respect with essence specification
validatePoint :: (MonadLog m,  EnumerateDomain m)
              => Model -> Provider -> Point
              -> m Bool
validatePoint vaildateSpec (Provider ps) (Point parts) = do
  let (givensP, findsP) = partition ( \(n, _) -> isJust  $ n `lookup` ps ) parts
  let givens =  pointToModel (Point givensP)
  let finds  =  pointToModel (Point findsP)

  x <- runExceptT $ ignoreLogs $ runNameGen $
         validateSolution vaildateSpec givens finds
  case x of
    Left{}  -> return False
    Right{} -> return True


-- | When a param that passed validate param
runParam :: (MonadIO m, MonadLog m)
         => Integer -> Int -> Int -> Method Smac -> Maybe RunMetadata
         -> m ()
runParam startOurCPU rTimestampStart _s_seed prevState prevMeta  = do
  (Method _ thisSmac) <- s_runMethod prevState
  out $line $ groom thisSmac
  let (thisQuality, thisTotals, raceTime) = fromJustNote "Need a result" $ sResult thisSmac

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
                   else
                       "SAT"

  let runtime = rOurCPUTime  + raceTime
  outputResult resultType runtime 0  (truncate smacQuality) _s_seed

-- | Parse the arguments
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

  -- Assumes the input is sorted

  parseSmacValues :: (MonadState [(Name,Expression)] m, MonadIO m)
                  => (Text, Domain () Constant, [(Text, Integer)])
                  -> m (Conjure.Language.Name.Name, Constant)
  parseSmacValues (name,DomainInt{},[(_,i)]) = do
    modify $ \st -> (Name name, Constant $ ConstantInt i) : st
    return (Name name, ConstantInt i)

  -- Basily Function1D
  parseSmacValues (name,
    (DomainFunction _ (FunctionAttr SizeAttr_None PartialityAttr_Total JectivityAttr_None)
                    (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt upper)])
                     DomainInt{}) , vs) = do
    let tuples = genericTake upper [ (parse "%FT%" t  , ConstantInt v) | (t,v) <- vs ]
    return $ (Name name, ConstantAbstract $ AbsLitFunction tuples)

  -- Decoding would work for any function (total) _ -> int
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

  --Used in prob048-meb
  parseSmacValues (name,DomainMatrix
    ix1@(DomainInt [RangeBounded (ConstantInt 1) (ConstantInt lim1)])
    (DomainMatrix
      ix2@(DomainInt [RangeBounded (ConstantInt 1) (ConstantInt lim2)])
      (DomainInt [RangeBounded (ConstantInt _) (ConstantInt _)])) , vs) = do
    let matrixes = [ doRange i   | i <- [1..lim1]  ]
    return $ (Name name, ConstantAbstract $ AbsLitMatrix ix1 matrixes)

    where
    doRange i =
      let pre  = (T.pack $ "%matrix2d%" ++ (zeroPad 2 (fromInteger i)) ++ "%" )
          vals = genericTake lim2
                 [ (parse pre t, v) | (t,v) <- vs, pre `T.isPrefixOf` t ]
      in ConstantAbstract $ AbsLitMatrix ix2 (map (ConstantInt . snd) vals)

  -- for the Problem Diagnosis problem
  parseSmacValues (name, DomainFunction ()
    (FunctionAttr SizeAttr_None PartialityAttr_Total JectivityAttr_None)
    (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt lim)])
    (DomainFunction ()
       (FunctionAttr SizeAttr_None PartialityAttr_Partial JectivityAttr_None)
       (DomainMatrix (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 2)]) DomainBool)
       DomainBool), vs) = do
    let vals =  genericTake lim [ (parse "%enumerated%" t, v ) | (t,v) <- vs ]
    let mappings = [(from, gateFunc_rangeEnumerated `at` (fromInteger toIx))
                   | (from, toIx) <- vals]

    return $ (Name name, ConstantAbstract $ AbsLitFunction mappings)

  parseSmacValues (name, DomainRelation ()
    (RelationAttr SizeAttr_None (BinaryRelationAttrs (barrs)))
    [DomainInt [RangeBounded (ConstantInt 1) (ConstantInt lim1)],
     DomainInt [RangeBounded (ConstantInt 1) (ConstantInt lim2)]] , vs)
     | S.null barrs  = do
    let tuples = mapMaybe doRange [1..(lim1 * lim2) ]
    out $line (groom tuples)
    return $ (Name name, ConstantAbstract $ AbsLitRelation tuples)

    where
    doRange i =
       let pre =  (T.pack $ "%rel%" ++ (zeroPad 3 (fromInteger i)) )
       in case [ v | (t,v) <- vs, pre == t  ] of
            [0] -> Nothing
            [1] -> Just [ ConstantInt v | (t,v) <- vs
                        , (T.concat [pre, "%"]) `T.isPrefixOf` t  ]
            val -> error $ $line ++ "parseSmacValues binRel: " ++ show val



  parseSmacValues (name, DomainFunction ()
    (FunctionAttr SizeAttr_None PartialityAttr_Total JectivityAttr_None)
    (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt lim1)])
    (DomainMatrix
    matIndexer@(DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 2)])
    (DomainInt [RangeBounded (ConstantInt (-1)) (ConstantInt lim2)])) , vs)
      | lim1 == lim2  = do

    let mappings = [ (ConstantInt i, doRange i )  | i <- [1..lim1]  ]
    return $ (Name name, ConstantAbstract $ AbsLitFunction mappings)

    where
    doRange i =
      let pre =  (T.pack $ "%FT_int_matrix%" ++ (zeroPad 2 (fromInteger i)) ++ "%" )
          vals = [ (parse pre t, v) | (t,v) <- vs, pre `T.isPrefixOf` t  ]
      in ConstantAbstract $ AbsLitMatrix matIndexer (map (ConstantInt . snd) vals)

  parseSmacValues (name, DomainFunction ()
    (FunctionAttr SizeAttr_None PartialityAttr_Partial JectivityAttr_None)
    (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt bound)])
    DomainBool , vs) = do

    let mappings = mapMaybe doRange [1..bound]
    return $ (Name name, ConstantAbstract $ AbsLitFunction mappings)

    where
    doRange i =
      let val = headNote $line [v | (t,v) <- vs
            ,(T.pack $ "%P_int_bool%" ++ (zeroPad 2 (fromInteger i)) ++ "%" )
                 `T.isInfixOf` t ]

      in Just (ConstantInt i, ConstantBool $ intToBool val)
  -- for the Problem Diagnosis problem

  parseSmacValues (name,dom,vs) = do
    lineError $line ["unhandled", nn "name" name, nn "dom" dom
                    , nn "dom" (groom dom),   nn "vs" (groom vs) ]


  intToBool 0 = False
  intToBool 1 = True
  intToBool x = error $ "intToBool not a bool" ++ show x

  parse :: Text -> Text -> Constant
  parse kind t = ConstantInt $  parseVal kind t

  parseVal :: forall a . Read a => Text -> Text -> a
  parseVal kind t =  fromJustNote ("Failed parsing Int" ++ show t) $ id
              <$> (T.stripPrefix kind t >>= return . T.unpack >>= readMay)


s_runInit ::( MonadIO m, MonadLog m)
            => Method Smac
            ->  m (Method Smac)
s_runInit state = do
  flip execStateT state $
    createParamEssence >> initDB >> doSaveEprimes True

-- | like run method but with some parts omitted
s_runMethod :: ( MonadIO m, MonadLog m)
            => Method Smac
            -> m (Method Smac)
s_runMethod state = do
  flip execStateT state $
    handleWDEG >> run

-- | Load RunMetadata if it exists
loadRunMetaData :: MonadIO m => m (Maybe RunMetadata)
loadRunMetaData = readFromJSONMay "metadata.json"

writeRunMetaData :: MonadIO m => RunMetadata -> m ()
writeRunMetaData rd = liftIO $ writeToJSON "metadata.json" rd

combineMeta :: Maybe RunMetadata -> RunMetadata -> Int -> Int -> Double
            -> RunMetadata
combineMeta Nothing rd  tsStart tsEnd thisOurCPUTime =
  rd{
    rTimestampStart = min (rTimestampStart rd) tsStart
  , rTimestampEnd   = max (rTimestampEnd rd) tsEnd
  , rOurCPUTime     = thisOurCPUTime
  , rCPUTime        = rCPUTime rd + (thisOurCPUTime -  rOurCPUTime rd)
  }

combineMeta (Just prev) rd tsStart tsEnd thisOurCPUTime =
  let xRacesCPUTime    = rRacesCPUTime rd
      xParamGenCPUTime = rParamGenCPUTime rd
      xSubCPUTime      = rSubCPUTime rd
      xOurCPUTime      = rOurCPUTime prev + thisOurCPUTime
      xCPUTime         = xRacesCPUTime + xParamGenCPUTime +  xOurCPUTime  + xSubCPUTime
  in
      RunMetadata
      { rTimestampStart                = min (rTimestampStart prev) tsStart
      , rTimestampEnd                  = max (rTimestampEnd rd) tsEnd
      , rRealTime                      = rRealTime prev + rRealTime rd
      , rIterationsDone                = rIterationsDone prev + rIterationsDone rd
      , rIterationsDoneIncludingFailed = rIterationsDoneIncludingFailed prev + rIterationsDoneIncludingFailed rd

      , rRacesCPUTime    = xRacesCPUTime
      , rParamGenCPUTime = xParamGenCPUTime
      , rSubCPUTime      = xSubCPUTime
      , rOurCPUTime      = xOurCPUTime
      , rCPUTime         = xCPUTime
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

zeroPad :: Int -> Int ->  String
zeroPad p n = replicate (p - length sn) '0'  ++ sn
 where sn = show n
