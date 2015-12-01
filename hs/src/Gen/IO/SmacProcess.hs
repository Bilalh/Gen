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
import Gen.Instance.RaceRunner          (initDB)
import Gen.Instance.Results.Results
import Gen.Instance.UI
import Gen.Instance.UI
import Gen.IO.FindCompact
import Gen.IO.Formats                   (allGivensOfEssence, getFullPath,
                                         readFromJSON, readFromJSONMay)
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

  -- TODO get prev RunMetadata
  startState <- loadState x point

  s_runMethod startState

  -- Read quality from db

  endOurCPU <- liftIO $ getCPUTime
  let rOurCPUTime = fromIntegral (endOurCPU - startOurCPU) / ((10 :: Double) ^ (12 :: Int))


  -- Update CPU Time and rewrite RunMetadata
  -- get SAT/Unsat


  runtime <- liftIO $ randomRIO (1,5)
  quality <- liftIO $ randomRIO  (20,90 :: Int)

  outputResult "SAT" runtime 0 quality s_seed


parseParamArray :: MonadIO m => [String] -> [(Text,Domain () Expression)]
                -> m  Point
parseParamArray arr givens = do
  let tuples = process arr
  out $line $  groom tuples
  let grouped = map (\(name,dom) -> (name,dom,
                                     [ (t,v)  | (t,v) <- tuples, name `T.isPrefixOf` t ]
                                    ) ) givens
  out $line $ groom grouped
  let vs = sort $ map parseSmacValues grouped

  return $ Point vs


  where
  process :: [String] -> [(Text,Integer)]
  process []       = []
  process [x]      = error $ "single element" ++ show x
  process (x:y:zs) = (T.pack $ tail x, fromJustNote "must be an Int" $ readMay y)
                   : process zs

  -- Parse the encoded values back to essence
  parseSmacValues (name,DomainInt{},[(_,i)]) = (Name name, ConstantInt i)


-- | like run method but with some parts omitted
s_runMethod :: ( MonadIO m, MonadLog m)
            => (Bool, Method Smac) ->  m ()
s_runMethod (initValues, state) = do
  void $ flip execStateT state $ do
    when initValues $ initDB >> doSaveEprimes True
    handleWDEG >> run


-- | Load the state from disk if it exists otherwise init it.
loadState :: MonadIO m => IN.CSV_IN -> Point -> m (Bool, Method Smac)
loadState dat point = liftIO $ doesFileExist "state.json" >>= \case
  False -> (\x -> (True, x))  <$> initState dat point
  True  -> do
    (Method common Smac{}) <- readFromJSON "state.json"
    return $ (False, Method common (Smac point))

initState :: MonadIO m => IN.CSV_IN -> Point -> m (Method Smac)
initState IN.CSV_IN{..} point = do
  essenceA <- liftIO $ getFullPath essence
  let info_path   = replaceFileName essenceA "info.json"
      models_path = replaceFileName essenceA (takeBaseName essenceA ++ "_" ++ mode)

  compactFirst <- lookupCompact models_path essenceA
  cores        <- liftIO $ fromJustNote "CORES must be set" <$> lookupEnv "CORES"

  i <- liftIO $ readFromJSON info_path
  p <- ignoreLogs $ makeProvider essenceA i
  out <- liftIO $ makeAbsolute "."

  let common          = MCommon{
        mEssencePath    = essenceA
      , mOutputDir      = out
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

  return $ Method common (Smac point)

{- Use the parsed point to run 1 race  -}

data Smac = Smac Point
  deriving (Eq, Show, Data, Typeable, Generic)

instance A.FromJSON Smac
instance A.ToJSON Smac

instance Sampling Smac where
  doIteration = do
    Method _ (Smac picked) <- get
    runParamAndStoreQuality picked >>= \case
      Left err -> return $ Left err
      Right{}  -> do
        storeDataPoint picked
        return $ Right ()


-- | This needs to be the last line
outputResult :: MonadIO m => String -> Double -> Int -> Int -> Int -> m ()
outputResult result_kind runtime runlength quality seed = do
  liftIO $ printf "Final Result for ParamILS: %s, %f, %d, %d, %d\n"
          result_kind runtime runlength quality seed

-- | Allows us to see the output in the logs
out :: MonadIO m => String -> String -> m ()
out l s = liftIO $ hPutStrLn stderr $ unlines $ ('»' : ' ' : l)
        : [ '»' : ' ' : xs | xs <- lines s ]
