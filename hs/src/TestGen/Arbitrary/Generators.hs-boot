{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Generators where

import TestGen.Helpers.StandardImports
import TestGen.Arbitrary.Data

atype :: GG TType
dom   :: GG (Domainn Expr)
useFunc :: FuncsNames -> GG Bool


instance Default SS

instance Default Generators