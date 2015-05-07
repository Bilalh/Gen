{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, ScopedTypeVariables#-}

{-# LANGUAGE LambdaCase, MultiWayIf, TemplateHaskell #-}

module Gen.Arbitrary.TypeConversions where

import Gen.Arbitrary.Prelude

toTypeWithConversions :: Type -> GG (Maybe (GG Expr) )
