module Gen.Reduce.QuanToComp(quanToComp, fromTo) where

import Gen.Prelude
import Conjure.Language.Definition
import Conjure.Language.NameResolution (resolveNames)

quanToComp :: MonadFail m => Spec -> m Spec
quanToComp = fromTo

fromTo :: MonadFail m => Spec -> m Spec
fromTo sp = do
  m :: Model <- toConjure sp
  with_names <- ignoreLogs . runNameGen $ resolveNames m
  fromConjure with_names
