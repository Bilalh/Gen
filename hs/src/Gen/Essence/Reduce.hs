{-# LANGUAGE RecordWildCards #-}
module Gen.Essence.Reduce(reduceErrors,reduceError, ReduceResult(..)) where

import Gen.Essence.Carry
import Gen.Essence.UIData       (EssenceConfig (..))
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.RunResult
import Gen.IO.Toolchain         (doMeta)
import Gen.IO.ToolchainData     (ToolchainOutput(ToolchainNull_))
import Gen.Reduce.Data          (RConfig (..), RState (..))
import Gen.Reduce.FormatResults (formatResults)
import Gen.Reduce.Random
import Gen.Reduce.Reduce        (reduceMain)
import System.FilePath          (takeBaseName)
import Control.Concurrent.ParallelIO.Global (parallelInterleaved)
import Gen.Helpers.MonadNote
import Gen.Essence.Log
import qualified Gen.Generalise.Data    as G

import qualified Gen.Reduce.Data as R

data ReduceResult = ReduceResult
    { finalSpec :: Spec
    } deriving Show


instance Pretty ReduceResult where
    pretty (ReduceResult{..}) = "ReduceResult" <+> pretty finalSpec


reduceErrors :: (MonadState Carry m, MonadIO m, MonadDB m)
             => EssenceConfig -> [ErrData] -> m [ReduceResult]
reduceErrors ec@EssenceConfig{logLevel} errs = do
  args <- mapM (reduceArgs ec) errs

  results <- liftIO $ do
    res <- parallelInterleaved [ processReduceArgs logLevel arg | arg <- args ]
    return res

  let (specs,dbs) = unzip results
  let finalDb = mconcat dbs
  putsDb finalDb
  return [ ReduceResult{finalSpec=sp} | sp <- specs]

reduceError :: (MonadState Carry m, MonadIO m, MonadDB m)
             => EssenceConfig -> ErrData -> m ReduceResult
reduceError ec err = do
   args     <- reduceArgs ec err
   (sp,db)  <- liftIO $ processReduceArgs (logLevel ec) args
   putsDb db
   return (ReduceResult sp)


-- Using IO so I can use parallel from Control.Concurrent.ParallelIO.Global
processReduceArgs :: LogLevel -> RState -> IO (Spec, ResultsDB)
processReduceArgs logLevel args = do
  theSeed :: Int <- randomRIO (0 ,2^(31 :: Int)-1)
  ((state,mlogs),lgs) <- runLogT logLevel $ runRndGen theSeed $ runNoteT
                      $ reduceMain False args

  (dir, lgs2) <- runNoteT $ formatResults True False state >>= \case
         (Just x) -> return x
         Nothing  -> return (specDir_ . rconfig $ args)

  let logsPath = (outputDir_ . rconfig $ args ) <.> ".logs"
  putStrLn $ "logs at " ++  logsPath
  logsToFile logsPath (lgs ++ lgs2 ++ mlogs)

  newDb <- missingToSkipped  (R.resultsDB_  state)
  sp <- readFromJSON (dir </> "spec.spec.json")

  return $ (sp, newDb)


reduceArgs :: (MonadIO m, MonadDB m)
           => EssenceConfig -> ErrData -> m RState
reduceArgs EssenceConfig{..} ErrData{..}= do
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
               ,R.toolchainOutput_   = ToolchainNull_
               ,R.deletePassing_     = deletePassing_
               ,resultsDB_dir        = db_dir
               ,alwaysCompact_       = False
               ,printState_          = True
               }
             ,resultsDB_           = db
             ,mostReducedChoices_  = Just choices
             ,timeLeft_            = total_time_may
             }
  liftIO $ doMeta out no_csv binariesDirectory_
  return args

generaliseArgs :: (MonadIO m, MonadDB m)
           => EssenceConfig -> ErrData -> m G.GState
generaliseArgs EssenceConfig{..} ErrData{..}= do
  db     <- getsDb
  db_dir <- getDbDirectory

  let per_spec_time  = round (fromIntegral perSpecTime_ * 1.5 :: Double) :: Int
      no_csv         = False
      total_time_may = reduceAsWell_
      out            = specDir ++ "_g-" ++ (replaceExtensions "" $ takeBaseName choices)

  let args = G.GState{ rconfig =
               RConfig
               {oErrKind_            = kind
               ,oErrStatus_          = status
               ,oErrChoices_         = Just choices
               ,outputDir_           = out
               ,specDir_             = specDir
               ,R.cores_             = 1
               ,specTime_            = per_spec_time
               ,R.binariesDirectory_ = binariesDirectory_
               ,R.toolchainOutput_   = ToolchainNull_
               ,R.deletePassing_     = deletePassing_
               ,resultsDB_dir        = db_dir
               ,alwaysCompact_       = False
               ,printState_          = True
               }
             ,resultsDB_           = db
             ,G.choicesToUse_      = Just choices
             ,G.otherErrors_       = []
             ,G.passingTrees       = []
             }
  liftIO $ doMeta out no_csv binariesDirectory_
  return args