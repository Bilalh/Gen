{-# LANGUAGE RecordWildCards #-}
module Gen.Essence.Reduce where

import Gen.Essence.UIData       (EssenceConfig (..))
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.Toolchain         (KindI, StatusI, doMeta)
import Gen.Reduce.Data          (RState (..))
import Gen.Reduce.FormatResults (formatResults)
import Gen.Reduce.Reduce        (reduceMain)
import System.FilePath          (takeBaseName)

import qualified Gen.Reduce.Data as R

data ErrData = ErrData
    { kind    :: KindI
    , status  :: StatusI
    , choices :: FilePath
    , specDir :: FilePath
    } deriving Show

instance Pretty ErrData where
    pretty = pretty . groom

data ReduceResult = ReduceResult
    { finalSpec :: Spec
    } deriving Show


instance Pretty ReduceResult where
    pretty (ReduceResult sp) = "ReduceResult" <+> pretty sp


reduceErrors :: EssenceConfig -> [ErrData] -> IO [ReduceResult]
reduceErrors ec = mapM (reduceError ec)

reduceError :: EssenceConfig -> ErrData -> IO ReduceResult
reduceError EssenceConfig{..} ErrData{..}= do
  db <- def

  let per_spec_time  = round (fromIntegral perSpecTime_ * 1.5 :: Double) :: Int
      no_csv         = False
      db_directory   = Nothing
      total_time_may = reduceAsWell_
      out            = specDir ++ "_r-" ++ (replaceExtensions "" $ takeBaseName choices)

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
                ,timeLeft_            = total_time_may
                ,totalIsRealTime_     = totalIsRealTime
                }

  doMeta out no_csv binariesDirectory_

  state <- reduceMain False args
  -- saveDB db_only_passing db_directory (resultsDB_  state)
  dir <- formatResults True state >>= \case
         (Just x) -> return x
         Nothing  -> return specDir

  sp <- readFromJSON (dir </> "spec.spec.json")

  return $ ReduceResult sp
