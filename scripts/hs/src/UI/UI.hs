{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs
module UI.UI where

import TestGen.Prelude

import TestGen.Helpers.Runner(KindI(..), StatusI(..))

import System.Console.CmdArgs hiding ( Default(..) )


data UI
  = Reduce
      { spec_directory :: FilePath
      -- , error_kind     :: KindI
      -- , error_status   :: StatusI

      -- , list_kinds    :: Bool
      -- , list_statuses :: Bool

      -- common
      -- , per_spec_time    :: Int
      , output_directory :: FilePath
      -- , old_conjure      :: Bool
      -- , cores            :: Int
      -- , seed             :: Maybe Int
      , limit_time :: Maybe Int
      }
  | Link
      { directories :: [FilePath]

      , limit_time :: Maybe Int
      }
  deriving (Show, Data, Typeable)

ui :: UI
ui  = modes

  [ Reduce
     { spec_directory   = def     &= typDir
                                  &= argPos 0
     , output_directory = def     &= typDir
                                  &= name "output-directory"
                                  &= name "o"
                                  &= groupname "IO"
                                  &= explicit
                                  &= help "Output directory "
     , limit_time       = Nothing &= name "limit-time"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Time limit in seconds. (CPU time)."

     } &= explicit
       &= name "reduce"
       &= help "Reduce a essence spec"

  , Link
    {
      directories = def &= typDir
                        &= name "directory"
                        &= name "d"
                        &= explicit
                        &= help "Directories containing spec.meta.json files "
    , limit_time  = def &= name "limit-time"
                        &= groupname "Other"
                        &= explicit
                        &= help "Time limit in seconds. (CPU time)."

    } &= explicit
      &= name "link"
      &= help "Classify the specs by creating symlinks"

  ] &= program "gen"
    &= summary "Gen the test case generator"
    &= helpArg [name "h"]
