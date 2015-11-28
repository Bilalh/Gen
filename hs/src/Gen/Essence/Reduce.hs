{-# LANGUAGE RecordWildCards #-}
module Gen.Essence.Reduce where

import Gen.Essence.Carry
import Gen.Essence.UIData       (EssenceConfig (..))
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.RunResult
import Gen.IO.Toolchain         (doMeta)
import Gen.Reduce.Data          (RConfig (..), RState (..))
import Gen.Reduce.FormatResults (formatResults)
import Gen.Reduce.Random
import Gen.Reduce.Reduce        (reduceMain)
import System.FilePath          (takeBaseName)

import qualified Gen.Reduce.Data as R

data ReduceResult = ReduceResult
    { finalSpec :: Spec
    } deriving Show


instance Pretty ReduceResult where
    pretty (ReduceResult sp) = "ReduceResult" <+> pretty sp


reduceErrors :: (MonadState Carry m, MonadIO m, MonadDB m)
             => EssenceConfig -> [ErrData] -> m [ReduceResult]
reduceErrors ec = mapM (reduceError ec)

reduceError :: (MonadState Carry m, MonadIO m, MonadDB m)
            => EssenceConfig -> ErrData -> m ReduceResult
reduceError EssenceConfig{..} ErrData{..}= do
  db     <- getsDb
  db_dir <- getDbDirectory

  let per_spec_time  = round (fromIntegral perSpecTime_ * 1.5 :: Double) :: Int
      no_csv         = False
      total_time_may = reduceAsWell_
      out            = specDir ++ "_r-" ++ (replaceExtensions "" $ takeBaseName choices)

  let args = def{ rconfig=
               RConfig
               {oErrKind_            = kind
               ,oErrStatus_          = status
               ,oErrChoices_         = Just choices
               ,outputDir_           = out
               ,specDir_             = specDir
               ,R.cores_             = 1
               ,specTime_            = per_spec_time
               ,R.binariesDirectory_ = binariesDirectory_
               ,R.toolchainOutput_   = toolchainOutput_
               ,R.deletePassing_     = deletePassing_
               ,totalIsRealTime_     = totalIsRealTime
               ,resultsDB_dir        = db_dir
               ,alwaysCompact_       = False
               }
             ,resultsDB_           = db
             ,mostReducedChoices_  = Just choices
             ,timeLeft_            = total_time_may
             }

  liftIO $ doMeta out no_csv binariesDirectory_

  state <- runLoggerPipeIO logLevel $ runRndGen 1 $ reduceMain False args
  writeDB_ False db_dir (resultsDB_  state)
  dir <- liftIO $ formatResults True False state >>= \case
         (Just x) -> return x
         Nothing  -> return specDir

  sp <- liftIO $ readFromJSON (dir </> "spec.spec.json")

  return $ ReduceResult sp
