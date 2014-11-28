{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module TestGen.Arbitrary.Generators where

import AST.Imports(Domain)
import TestGen.Arbitrary.Data
import Data.Default as X ( Default)

atype :: GG Type 
dom   :: GG Domain

instance Default SS

instance Default Generators