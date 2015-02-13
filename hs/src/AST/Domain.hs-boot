{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module AST.Domain where

import Conjure.Prelude
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.Definition

import AST.ToEssence(ToEssence(..))
import AST.FromEssence(FromEssence(..))
import AST.Data

instance ToEssence DDomain (Domain () Expression)
instance FromEssence (Domain () Expression) DDomain
instance Pretty DDomain

dint, dset, dmset, dmat, dfunc, drel, dpar, dtuple  :: DDomain
dintRRange :: Integer -> Integer -> DDomain