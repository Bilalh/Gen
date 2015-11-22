{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
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
import Gen.IO.Formats                   (readFromJSON, readFromJSONMay)
import System.Directory                 (getHomeDirectory)
import System.Exit                      (ExitCode (ExitSuccess))
import System.FilePath                  (takeDirectory)
import  Gen.Instance.Results.Results

import qualified Data.Vector              as V
import qualified Data.Map                 as M
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import qualified Gen.Instance.Results.SettingsIn  as IN


smacProcess s_output_directory s_eprime s_instance_specific
  s_cutoff_time s_cutoff_length s_seed s_param_arr = do

  vs <- liftIO $ V.toList <$> decodeCSV (s_output_directory </> "settings.csv")
  let x@IN.CSV_IN{..} = headNote "setting.csv should have one row" vs

  liftIO $ print "smacProcess"
  liftIO $ groomPrint x
