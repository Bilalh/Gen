{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Gen.IO.SmacProcess where

import Conjure.Language.Definition
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
import Gen.Instance.Point
import Gen.Instance.Results.Results
import Gen.Instance.Method
import Gen.Instance.UI
import Gen.IO.Formats                   (readFromJSON, readFromJSONMay)
import System.CPUTime                   (getCPUTime)
import System.Directory                 (getHomeDirectory)
import System.Exit                      (ExitCode (ExitSuccess))
import System.FilePath                  (takeDirectory)
import Text.Printf
import Gen.Instance.Undirected

import qualified Data.Map                        as M
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import qualified Data.Vector                     as V
import qualified Gen.Instance.Results.SettingsIn as IN


smacProcess s_output_directory s_eprime s_instance_specific
  s_cutoff_time s_cutoff_length s_seed s_param_arr = do
  startOurCPU <- liftIO $  getCPUTime

  vs <- liftIO $ V.toList <$> decodeCSV (s_output_directory </> "settings.csv")
  let x@IN.CSV_IN{..} = headNote "setting.csv should have one row" vs

  liftIO $ print "smacProcess"

  endOurCPU <- liftIO $ getCPUTime
  let rOurCPUTime = fromIntegral (endOurCPU - startOurCPU) / ((10 :: Double) ^ (12 :: Int))
  liftIO $ groomPrint x

  runtime <- liftIO $ randomRIO (1,5)
  quality <- liftIO $ randomRIO  (20,90 :: Int)

  outputResult "SAT" runtime 0 quality s_seed

-- TODO sum with prev RunMetadata

-- | Load the state from disk if it exists otherwise init it.
s_loadState :: MonadIO m => IN.CSV_IN  -> m (Method Undirected)
s_loadState dat = liftIO $ doesFileExist "state.json" >>= \case
  False -> return $ s_initState dat
  True  -> readFromJSON "state.json"

s_initState :: IN.CSV_IN -> Method Undirected
s_initState = $notDone


s_runMethod :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m, ToJSON a)
            => m ()
s_runMethod = do
  $notDone

-- | This needs to be the last line
outputResult :: MonadIO m => String -> Double -> Int -> Int -> Int -> m ()
outputResult result_kind runtime runlength quality seed = do
  liftIO $ printf "Final Result for ParamILS: %s, %f, %d, %d, %d\n"
          result_kind runtime runlength quality seed
