{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, ScopedTypeVariables#-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE LambdaCase, MultiWayIf, TemplateHaskell #-}

module TestGen.Arbitrary.TypeConversions where

import TestGen.Arbitrary.Helpers.Prelude
import TestGen.Arbitrary.Expr

toTypeWithConversions :: Type -> GG (Maybe Expr)
