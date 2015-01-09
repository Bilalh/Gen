{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Generators where

import AST.Imports(Domain)
import TestGen.Arbitrary.Data
import Data.Default as X ( Default)

atype :: GG Type
dom   :: GG Domain
useFunc :: FuncsNames -> GG Bool


instance Default SS

instance Default Generators