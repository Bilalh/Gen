module Gen.Instance.RaceRunner where

import Conjure.Language
import Conjure.Language.Expression.DomainSizeOf (domainSizeOf)
import Conjure.Language.NameResolution          (resolveNames)
import Conjure.UI.IO
import Gen.Imports
import Gen.Instance.Data
import Gen.IO.Formats
import Gen.IO.Toolchain                         (runCommand)
import System.Directory                         (renameFile)
import System.Exit                              (ExitCode (..))
import System.IO.Temp                           (withSystemTempDirectory)

import qualified Data.Set as S

createParamEssence ::  (Sampling a, MonadState (Method a) m, MonadIO m) => m ()
createParamEssence = do
  (Method MCommon{mEssencePath,mVarInfo, mOutputDir} _) <- get
  let specFp   = (mOutputDir </> "essence_param_find.essence")
  let eprimeFp = (mOutputDir </> "essence_param_find.eprime")

  liftIO $ doesFileExist specFp >>= \case
    True  -> return ()
    False -> do
      model     <- liftIO $ readModelFromFile mEssencePath
      paramSpec <- liftIO $ createParamSpecification model mVarInfo
      writeModel PlainEssence (Just "essence_param_find.essence") paramSpec

  liftIO $ doesFileExist eprimeFp >>= \case
    True  -> return ()
    False -> do
      conjureCompact specFp eprimeFp >>= \case
        True  -> return ()
        False -> userErr1 "Failed to refine the param specification, namely essence_param_find.essence "


sampleParamFromMinion :: (Sampling a, MonadState (Method a) m, MonadIO m) => Point -> m ()
sampleParamFromMinion = $notDone

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
