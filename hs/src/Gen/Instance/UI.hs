module Gen.Instance.UI where

import Conjure.Language
import Conjure.Language.NameResolution   (resolveNames)
import Conjure.UI.IO
import Conjure.UI.TypeCheck
import Gen.Imports
import Gen.Instance.BuildDependencyGraph
import Gen.Instance.Data
import Gen.Instance.Method
import Gen.Instance.Point
import Gen.Instance.RaceRunner
import Gen.Instance.SamplingError
import Gen.IO.Formats
import System.Directory                  (makeAbsolute)
import System.FilePath                   (replaceFileName, takeBaseName)
import System.Random                     (mkStdGen, setStdGen)

import qualified Data.Set as S

-- | The starting point for instance generation
runMethod :: (Sampling a, ToJSON a, MonadIO m) => Int -> LogLevel -> Method a -> m ()
runMethod = runMethod' True

runMethod' :: (Sampling a, ToJSON a, MonadIO m)
           => Bool -> Int -> LogLevel -> Method a -> m ()
runMethod' doEprimes seed lvl state= do
  liftIO $ setStdGen (mkStdGen seed)
  runLoggerPipeIO lvl $
    void $ flip execStateT state $
     createParamEssence >> initDB >> doSaveEprimes doEprimes >> run

doSaveEprimes :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
              => Bool -> m ()
doSaveEprimes False = return ()
doSaveEprimes True  = saveEprimes

findDependencies :: (MonadIO m, MonadLog m)
                 => FilePath -> FilePath
                 -> m (Either SamplingErr (VarInfo, Double))
findDependencies outBase specFp = do
  model <-  liftIO $ ignoreLogs $ readModelFromFile specFp
              >>= runNameGen . typeCheckModel_StandAlone
              >>= runNameGen .  resolveNames
  buildDependencyGraph outBase model

-- | Make the value provider for the givens
makeProvider :: (MonadIO m, MonadLog m) => FilePath -> VarInfo ->  m Provider
makeProvider fp  VarInfo{..} = do
  model <-  liftIO $ ignoreLogs $ readModelFromFile fp
               >>= runNameGen . typeCheckModel_StandAlone
               >>= runNameGen .  resolveNames


  vs <- core model
  cons <- liftIO $ forM vs $ \(n,expr) -> do
          v <- mapM e2c expr
          return (n, v)
  return $ Provider cons

  where
  core m = do
    vs <- forM (mStatements m) $ \ st -> case st of
      Declaration (FindOrGiven Given nm@(Name te) dom) ->
            if te `S.member` givens
            then return $ Just (nm, dom)
            else return $ Nothing
      _ -> return Nothing

    return $ catMaybes vs

_f1 :: IO (Either SamplingErr (VarInfo, Double))
_f1 = do
  createDirectoryIfMissing True _ex_out
  runLoggerPipeIO LogDebug $ findDependencies _ex_out _ex_essence

-- for examples
_ex_info, _ex_essence, _ex_out, _ex_mode, _ex_dir, _ex_prob :: String
_ex_prob    = "prob013-PPP"
_ex_mode    = "sample-64"
_ex_out     = "/Users/bilalh/CS/gen/__"
_ex_dir     = "/Users/bilalh/CS/essence-refinements/_current"
_ex_info    = _ex_dir </> _ex_prob </> "info.json"
_ex_essence = _ex_dir </> _ex_prob </> _ex_prob <.> ".essence"

-- Common settings
_ex_common :: IO MCommon
_ex_common = do
  i :: VarInfo <- readFromJSON _ex_info
  ess <- makeAbsolute _ex_essence
  p <- ignoreLogs $ makeProvider _ex_essence i
  let mModelsDir = replaceFileName ess (takeBaseName  _ex_essence ++ "_" ++ _ex_out)
  let common            = MCommon{
        mEssencePath    = _ex_essence
      , mOutputDir      = _ex_out
      , mModelTimeout   = 30
      , mVarInfo        = i
      , mPreGenerate    = Nothing
      , mIterations     = 3
      , mMode           = _ex_mode
      , mGivensProvider = p
      , mPoints         = []
      , mCores          = 2
      , mCompactName    = Nothing
      , mSubCpu  = 0
      , mModelsDir
      , mPointsGiven = Nothing
      }

  return common
