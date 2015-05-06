{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen.AST.Type where

import Conjure.Language.Name
import Conjure.Language.Pretty
import Conjure.Language.Type
import Conjure.Prelude
import Gen.AST.Data

instance Translate Text Name where
    toConjure x          = pure $ Name x
    fromConjure (Name x) = pure x
