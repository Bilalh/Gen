module Gen.Instance.RaceRunner(
    runRace
  , createParamEssence
  , sampleParamFromMinion
  ) where

import Conjure.Language
import Conjure.Language.Expression.DomainSizeOf (domainSizeOf)
import Conjure.Language.NameResolution          (resolveNames)
import Conjure.UI.IO
import Gen.Imports
import Gen.Instance.Data
import Gen.IO.Formats
import Gen.IO.Toolchain                         (runCommand,runCommand')
import System.Directory                         (renameFile)
import System.Exit                              (ExitCode (..))
import System.IO.Temp                           (withSystemTempDirectory)
import System.Environment(lookupEnv)
import System.FilePath(takeDirectory)

import qualified Data.Set as S

type ParamFP = FilePath

runRace :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
        => ParamFP -> m ()
runRace paramFP = do
  (Method MCommon{mEssencePath, mOutputDir, mModelTimeout} _) <- get
  now <- timestamp

  let args = [ show now
             , paramFP
             , show mModelTimeout
             , takeDirectory mEssencePath
             ]

  let env = [ ("NUM_JOBS", show 2)
            , ("USE_MODE", "df")
            , ("OUT_BASE_DIR", mOutputDir)
            , ("LIMIT_MODELS", "3")  -- Only race the first 3 models
            ]

  cmd <- wrappers "run.sh"
  res <- runCommand' (Just env) cmd args Nothing
  return ()


-- To parse results:
-- Need to do this first globally
-- cabal install split

createParamEssence :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m )
                   => m ()
createParamEssence = do
  logDebug "createParamEssence"
  (Method MCommon{mEssencePath,mVarInfo, mOutputDir} _) <- get
  let specFp   = (mOutputDir </> "essence_param_find.essence")
  let eprimeFp = (mOutputDir </> "essence_param_find.eprime")
  liftIO $ createDirectoryIfMissing True mOutputDir


  liftIO $ doesFileExist specFp >>= \case
    True  -> return ()
    False -> do
      model     <- liftIO $ readModelFromFile mEssencePath
      paramSpec <- liftIO $ createParamSpecification model mVarInfo
      writeModel PlainEssence (Just $ mOutputDir </> "essence_param_find.essence") paramSpec

  liftIO $ doesFileExist eprimeFp >>= \case
    True  -> return ()
    False -> do
      conjureCompact specFp eprimeFp >>= \case
        True  -> return ()
        False -> userErr1 "Failed to refine the param specification, namely essence_param_find.essence "


createParamSpecification :: (MonadUserError m, MonadFail m) => Model -> VarInfo -> m Model
createParamSpecification model VarInfo{..} = do
  let f = runNameGen . resolveNames  >=> core
  ignoreLogs $ f model

  where
  core m = do
    (outStatements, errs) <- runWriterT $ forM (mStatements m) $ \ st -> case st of
      Declaration (FindOrGiven Given nm@(Name te) dom) ->
        case domainSizeOf dom of
          Nothing -> tell [(nm, dom)] >> return []
          Just (_ :: Expression) -> do
            let k = if te `S.member` givens then Given else Find
            return [Declaration (FindOrGiven k nm dom)]

      Declaration (FindOrGiven Find _  _  ) -> return []
      Declaration {}                        -> return [st]
      SearchOrder {}                        -> return []
      Where       xs                        -> return [SuchThat xs]
      Objective   {}                        -> return []
      SuchThat    {}                        -> return []

    if null errs
      then return m { mStatements = concat outStatements }
      else userErr1 $ vcat $ "Given must have a finite domain"
                           : [ pretty nm <> ":" <++> pretty dom
                           | (nm, dom) <- errs
                           ]

conjureCompact :: MonadIO m => FilePath -> FilePath -> m Bool
conjureCompact inn out = do
  liftIO $ withSystemTempDirectory "gen-compact" $ \tmp -> do
    let args = ["-qf", "-ac", inn, "-o", tmp]
    runCommand  "conjure" args Nothing >>= \case
      (ExitFailure _) -> return False
      ExitSuccess     -> do
        renameFile (tmp </> "model000001.eprime") out
        return True


sampleParamFromMinion :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
                      => m ()
sampleParamFromMinion = do
  (Method MCommon{mOutputDir} _) <- get
  let seed = 4 :: Int
  now <- timestamp
  let out = mOutputDir </> "_param_gen" </> show now
  let timeout = 300 :: Int
  let paramName = "empty"
  let paramFp = (out </> paramName) <.> ".param"
  let solutionFp = out </> ("essence_param_find"  ++ "-" ++ paramName  <.> ".solution" )

  liftIO $ createDirectoryIfMissing True out
  writeParam [] paramFp

  let args = [ (mOutputDir </> "essence_param_find.essence")
             , (mOutputDir </> "essence_param_find.eprime")
             , paramFp
             , show timeout
             , show timeout
             , show seed
             ]
  let env = [ ("GENERATED_OUTPUT_DIR", out)
            , ("TIMEOUT5_FILE", out </> "timeout_file")
            ]
  cmd <- wrappers "create_param_from_essence.sh"
  res <- runCommand' (Just env) cmd args Nothing

  solution_param <- liftIO $ readModelFromFile solutionFp

  logDebug ("produced" <+> pretty solution_param)
  return ()

wrappers :: MonadIO m => FilePath -> m FilePath
wrappers fp = do
  liftIO $ lookupEnv ("PARAM_GEN_SCRIPTS" :: String) >>= \case
            Nothing -> liftIO $ error "No PARAM_GEN_SCRIPTS variable"
            Just p -> do
                return $ p </> "wrappers" </> fp

writeParam :: MonadIO m => [()] -> FilePath -> m ()
writeParam _ fp = do
  let m :: Model = def
  liftIO $ writeModel PlainEssence (Just fp) m


_ex1, _ex2 :: IO ()
_ex2 = do
  res <- conjureCompact "/Users/bilalh/CS/instancegen-models/_new/prob013-PPP/prob013-PPP.essence"
                        "/Users/bilalh/CS/instancegen-models/_new/prob013-PPP/a.eprime"
  print res

_ex1 = do
  i :: VarInfo <- readFromJSON "/Users/bilalh/CS/instancegen-models/_new/prob013-PPP/info.json"
  m <- readModelFromFile "/Users/bilalh/CS/instancegen-models/_new/prob013-PPP/prob013-PPP.essence"
  param <- createParamSpecification m i
  print . pretty $ param
