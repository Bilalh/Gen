module Gen.Reduce.TypeCheck where

import Conjure.Language.Definition
import Gen.Imports
import Conjure.Language.NameResolution (resolveNames)
import Conjure.UI.TypeCheck            (typeCheckModel)
import Data.Time.Clock.POSIX           (getPOSIXTime)
import Gen.Reduce.Data
import Gen.IO.ToolchainData


typeCheck :: MonadFail m => Model -> m Model
typeCheck m = ignoreLogs . runNameGen  $ (resolveNames $ m) >>= typeCheckModel


typeCheckWithResult :: MonadIO m => FilePath -> Model -> m ((Bool, RunResult),Int)
typeCheckWithResult dir model = do
  startTime <- liftIO $ round `fmap` getPOSIXTime

  let (res :: Either Doc Model) =  typeCheck  model

  endTime <- liftIO $ round `fmap` getPOSIXTime
  let realTime = endTime - startTime
  case res of
    Right{}   -> return ((False,Passing(realTime)),realTime)
    (Left errDoc) -> do
      liftIO $ writeFile (dir </> "spec.error") $ show . vcat $
                    [ errDoc
                    , "----"
                    , pretty model
                    ]
      liftIO $ writeFile (dir </> "model000000.choices.json") "{}"
      let ret = OurError{
                    resDirectory_  = dir
                  , resErrKind_    = TypeCheck_
                  , resErrStatus_  = TypeChecking_
                  , timeTaken_     = realTime
                  , resErrChoices_ = dir </> "model000000.choices.json"
                  }
      return ((True,ret),realTime)
