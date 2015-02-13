{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, ScopedTypeVariables#-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE LambdaCase, MultiWayIf, TemplateHaskell #-}

module TestGen.Arbitrary.TypeConversions where

import TestGen.Prelude

toTypeWithConversions :: TType -> GG (Maybe (GG Expr) )
