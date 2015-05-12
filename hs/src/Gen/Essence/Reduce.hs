{-# LANGUAGE  RecordWildCards #-}
module Gen.Essence.Reduce where

import Gen.Essence.UIData (EssenceConfig(..))
import Gen.Imports
import Gen.IO.Toolchain   (KindI, StatusI)
import Gen.Reduce.Data(RState(..))
import qualified Gen.Reduce.Data as R
import Gen.Reduce.Reduce(reduceMain)
import Gen.Reduce.FormatResults(formatResults)
import Gen.IO.Toolchain(doMeta)
import Gen.Reduce.Runner   (giveDb, saveDB)
import qualified Data.HashMap.Strict as H

data ErrData = ErrData
    { kind    :: KindI
    , status  :: StatusI
    , choices :: FilePath
    , specDir :: FilePath
    }

data ReduceResult = ReduceResult


reduceErrors :: EssenceConfig -> [ErrData] -> IO [ReduceResult]
reduceErrors ec = mapM (reduceError ec)

reduceError :: EssenceConfig -> ErrData -> IO ReduceResult
reduceError EssenceConfig{..} ErrData{..}= do
  db <- return H.empty

  let per_spec_time  = 30
      no_csv         = False
      db_directory   = Nothing
      total_time_may = Just 60
      out            = specDir ++ "_r"

  let args = def{oErrKind_            = kind
                ,oErrStatus_          = status
                ,oErrChoices_         = Just choices
                ,outputDir_           = out
                ,specDir_             = specDir
                ,R.cores_             = 1
                ,rgen_                = R.mkrGen (seed_)
                ,specTime_            = per_spec_time
                ,R.binariesDirectory_ = binariesDirectory_
                ,R.toolchainOutput_   = toolchainOutput_
                ,R.deletePassing_     = deletePassing_
                ,resultsDB_           = db
                ,mostReducedChoices_  = Just choices
                ,resultsDB_dir        = db_directory
                ,timeLeft_            =  total_time_may
                }

  doMeta out no_csv binariesDirectory_

  state <- reduceMain args
  -- saveDB db_only_passing db_directory (resultsDB_  state)
  formatResults True state
  return ReduceResult
