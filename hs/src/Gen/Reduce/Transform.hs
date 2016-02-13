module Gen.Reduce.Transform(quanToComp, deEnum) where

import Conjure.Language.Definition
import Conjure.Language.NameResolution (resolveNames)
import Conjure.UserError               (MonadUserError)
import Gen.Imports
import Conjure.Process.Enums
import Conjure.UI.IO
import Gen.Instance.Point


quanToComp ::  (MonadFail m, MonadUserError m, MonadIO m) => Spec -> m Spec
quanToComp = fromTo

fromTo :: (MonadFail m, MonadUserError m, MonadIO m) => Spec -> m Spec
fromTo sp = do
  m :: Model <- toConjure sp
  named <- ignoreLogs . runNameGen $ resolveNames m
  fromConjure named

deEnum ::  (MonadFail m, MonadUserError m, MonadIO m)
       => Spec -> Maybe ParamFP -> m (Spec, Maybe Point)
deEnum sp paramFp = do
  m :: Model <- toConjure sp
  no_enums <- ignoreLogs $ removeEnumsFromModel m

  named     <- ignoreLogs . runNameGen $  resolveNames no_enums
  namedSpec <- fromConjure named

  case paramFp of
    Nothing -> return (namedSpec, Nothing)
    Just fp -> do
      param <- readModelFromFile fp
      fixed  <- ignoreLogs $  removeEnumsFromParam named param
      return (namedSpec, Just (modelToPoint fixed))
