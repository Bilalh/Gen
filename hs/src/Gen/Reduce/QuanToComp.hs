module Gen.Reduce.QuanToComp(quanToComp, fromTo) where

import Conjure.Language.Definition
import Conjure.Language.NameResolution (resolveNames)
import Conjure.UserError               (MonadUserError)
import Gen.Imports


quanToComp ::  (MonadFail m, MonadUserError m, MonadIO m) => Spec -> m Spec
quanToComp = fromTo

fromTo :: (MonadFail m, MonadUserError m, MonadIO m) => Spec -> m Spec
fromTo sp = do
  m :: Model <- toConjure sp
  named <- ignoreLogs . runNameGen $ resolveNames m
  fromConjure named
