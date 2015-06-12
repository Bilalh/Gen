module Gen.Reduce.QuanToComp(quanToComp, fromTo) where

import Conjure.Language.Definition
import Conjure.Language.NameResolution (resolveNames)
import Conjure.UserError               (MonadUserError)
import Gen.Imports

quanToComp ::  (MonadFail m, MonadUserError m) => Spec -> m Spec
quanToComp = fromTo

fromTo :: (MonadFail m, MonadUserError m) => Spec -> m Spec
fromTo sp = do
  m :: Model <- toConjure sp
  with_names <- ignoreLogs . runNameGen $ resolveNames m
  fromConjure with_names
