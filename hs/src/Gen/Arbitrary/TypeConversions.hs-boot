{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, ScopedTypeVariables#-}

{-# LANGUAGE LambdaCase, MultiWayIf, TemplateHaskell #-}

module Gen.Arbitrary.TypeConversions where

import Gen.Prelude

toTypeWithConversions :: TType -> GG (Maybe (GG Expr) )
