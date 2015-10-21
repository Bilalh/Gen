module Gen.Instance.UI where

import Conjure.Language
import Conjure.Language.NameResolution   (resolveNames)
import Conjure.UI.IO
import Gen.Imports
import Gen.Instance.BuildDependencyGraph
import Gen.Instance.Data
import Gen.Instance.Method
import Gen.Instance.RaceRunner
import Gen.IO.Formats
import System.Directory                  (makeAbsolute)
import System.FilePath                   (replaceFileName, takeBaseName)
import System.Random                     (mkStdGen, setStdGen)
import Gen.Instance.SamplingError
import Conjure.UI.TypeCheck

import qualified Data.Set as S

-- The starting point for instance generation
runMethod :: (Sampling a, ToJSON a, MonadIO m) => Int -> LogLevel -> Method a -> m ()
runMethod seed lvl state= do
  liftIO $ setStdGen (mkStdGen seed)
  runLoggerPipeIO lvl $
    void $ flip execStateT state $
     createParamEssence >> initDB >> saveEprimes >> run


findDependencies :: (MonadIO m, MonadLog m)
                 => FilePath -> FilePath
                 -> m (Either SamplingErr (VarInfo, Double))
findDependencies outBase fp = do
  model <-  liftIO $ ignoreLogs $ readModelFromFile fp
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


-- for examples
_ex_info, _ex_essence, _ex_out, _ex_mode, _ex_dir, _ex_prob :: String
_ex_prob    = "prob006-GR"
_ex_mode    = "df"
_ex_out     = "/Users/bilalh/CS/gen/__"
_ex_dir     = "/Users/bilalh/CS/essence-refinements/_current"
_ex_info    = _ex_dir </> _ex_prob </> "info.json"
_ex_essence = _ex_dir </> _ex_prob </> _ex_prob <.> ".essence"
_ex_point  :: Point
_ex_point   = Point [("n", ConstantInt 4)] -- GR

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
      }

  return common
