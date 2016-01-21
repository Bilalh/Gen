module Gen.Instance.UI where

import Conjure.Language
import Conjure.Language.NameResolution   (resolveNames)
import Conjure.UI.IO
import Conjure.UI.TypeCheck
import Gen.Essence.Log
import Gen.Imports
import Gen.Instance.BuildDependencyGraph
import Gen.Instance.Data
import Gen.Instance.Method
import Gen.Instance.NoRacing             (NoRacing (..))
import Gen.Instance.Point
import Gen.Instance.RaceRunner
import Gen.Instance.SamplingError
import Gen.IO.Formats
import Shelly                            (which)
import System.Directory                  (makeAbsolute)
import System.Environment                (lookupEnv, setEnv, unsetEnv)
import System.FilePath                   (replaceFileName, takeBaseName,
                                          takeDirectory)
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
    handleWDEG >> createParamEssence >> initDB >> doSaveEprimes doEprimes >> run


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
makeProvider :: (MonadIO m, MonadLog m)
             => FilePath -> VarInfo ->  m (Provider, Set GivenName)
makeProvider fp  VarInfo{..} = do
  model <-  liftIO $ ignoreLogs $ readModelFromFile fp
               >>= runNameGen . typeCheckModel_StandAlone
               >>= runNameGen .  resolveNames


  vs <- core model
  cons <- liftIO $ forM vs $ \(n,expr) -> do
          v <- mapM e2c expr
          return (n, v)
  let findsNames  = S.fromList $ [ nm | Declaration (FindOrGiven Given nm@Name{} _)
                                      <- mStatements model ]
  return $ (Provider cons, findsNames)

  where
  core m = do
    vs <- forM (mStatements m) $ \ st -> case st of
      Declaration (FindOrGiven Given nm@(Name te) dom) ->
            if te `S.member` givens
            then return $ Just (nm, dom)
            else return $ Nothing
      _ -> return Nothing

    return $ catMaybes vs

handleWDEG :: (MonadState (Method a) m, MonadIO m, MonadLog m )
           => m ()
handleWDEG = do
  (Method MCommon{mEssencePath} _) <- get

  model <-  liftIO $ ignoreLogs $ readModelFromFile mEssencePath
  as <- forM (mStatements model) $ \st -> case st of
         (SearchHeuristic x) -> return $ Just $ x
         _ -> return Nothing
  case catMaybes as of
    []        -> return ()
    [x] -> when (x `elem` ["wdeg", "domoverwdeg"]) $ liftIO $ do
                  sh  $ do
                   which "minion-wdeg" >>= \case
                         Just{}  -> return ()
                         Nothing -> error "minion-wdeg need to be in the the path"
                  setEnv "MINION_BINARY" "minion-wdeg"

    xs  -> lineError $line $ "Got multiple heuristics " :map pretty xs


instances_no_racing :: FilePath -> Int -> Int -> FilePath -> Maybe FilePath
                    -> Int -> LogLevel -> IO ()
instances_no_racing essence_path iterations param_gen_time out customParamEssence
                    seed log_level= do
  let info_path   = replaceFileName essence_path "info.json"

  i :: VarInfo <- doesFileExist info_path >>= \case
    True -> readFromJSON info_path
    False -> do

      e <- lookupEnv "NULL_runPadded" >>= \case
        Just x  -> setEnv "NULL_runPadded" "true" >> return (Just x)
        Nothing -> setEnv "NULL_runPadded" "true" >> return Nothing

      res <- runLogT LogDebugVerbose (findDependencies out essence_path) >>= \case
        (Right (vi,_),_) -> return vi
        (Left err,lgs) -> docError $
          "Error: could not find the dependencies automatically and no info.json exists next to the essence spec."
          : nn "err"  err
          : map pretty lgs
      case e of
        Nothing -> unsetEnv "NULL_runPadded"
        Just x  -> setEnv "NULL_runPadded" x
      return res

  putStrLn $ "VarInfo: " ++ (groom i)
  (p,pnames) <- ignoreLogs $ makeProvider essence_path i
  putStrLn $ "Provider: " ++ (groom p)

  let   common              = MCommon{
        mEssencePath        = essence_path
      , mOutputDir          = out
      , mModelTimeout       = -1
      , mVarInfo            = i
      , mPreGenerate        = Nothing
      , mIterations         = iterations
      , mMode               = ""
      , mModelsDir          = ""
      , mGivensProvider     = p
      , mGivenNames         = pnames
      , mPoints             = []
      , mCores              = -1
      , mCompactName        = Nothing
      , mSubCpu             = 0
      , mPointsGiven        = Nothing
      , mParamGenTime       = param_gen_time
      , mCustomParamEssence = customParamEssence
      }

  runMethod' False seed log_level (Method common NoRacing)


-- for ghci

build_dependencies_of_csplib_essence :: IO ()
build_dependencies_of_csplib_essence = do
  let baseOut = "/Users/bilalh/Downloads/Essence/"
  files <- getAllFilesWithSuffix ".essence" baseOut
  setEnv "NULL_runPadded" "true"
  forM_ files $ \essence_fp -> do
    res <- runLoggerPipeIO LogNone $ findDependencies (takeDirectory essence_fp) essence_fp
    print res
    putStrLn "\n"


compare_to_manual_info :: IO ()
compare_to_manual_info = do
  let baseOut = "/Users/bilalh/CS/gen/__/compare"
  createDirectoryIfMissing True baseOut >> removeDirectoryRecursive baseOut

  files <- getAllFilesWithSuffix "info.json" "/Users/bilalh/CS/essence-refinements/_current"
  setEnv "NULL_runPadded" "true"
  forM_ files $ \info_fp -> do
    let dir = takeDirectory info_fp
    let ess = dir </> takeBaseName dir <.> ".essence"
    let out = baseOut </> takeBaseName dir
    createDirectoryIfMissing True out
    when ( "prob" `isPrefixOf` (takeBaseName dir) ) $ do
      print info_fp
      manual :: VarInfo <-  readFromJSON info_fp
      print manual
      res <- runLoggerPipeIO LogNone $ findDependencies out ess
      print res
      putStrLn "\n"

_f1 :: IO (Either SamplingErr (VarInfo, Double))
_f1 = do
  createDirectoryIfMissing True _ex_out
  runLoggerPipeIO LogDebugVerbose $ findDependencies _ex_out _ex_essence

_ex_info, _ex_essence, _ex_out, _ex_mode, _ex_dir, _ex_prob :: String
_ex_prob    = "prob030-BACP"
_ex_dir     = "/Users/bilalh/CS/essence-refinements/_current"
-- _ex_prob    = "ordering1"
-- _ex_dir     = "/Users/bilalh/CS/essence-refinements/zz"
_ex_mode    = "sample-64"
_ex_out     = "/Users/bilalh/CS/gen/__"
_ex_info    = _ex_dir </> _ex_prob </> "info.json"
_ex_essence = _ex_dir </> _ex_prob </> _ex_prob <.> ".essence"

-- Common settings
_ex_common :: IO MCommon
_ex_common = do
  i :: VarInfo <- readFromJSON _ex_info
  ess <- makeAbsolute _ex_essence
  (p, pnames) <- ignoreLogs $ makeProvider _ex_essence i
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
      , mGivenNames     = pnames
      , mPoints         = []
      , mCores          = 2
      , mCompactName    = Nothing
      , mSubCpu  = 0
      , mModelsDir
      , mPointsGiven = Nothing
      , mParamGenTime = 300
      , mCustomParamEssence = Nothing
      }

  return common
