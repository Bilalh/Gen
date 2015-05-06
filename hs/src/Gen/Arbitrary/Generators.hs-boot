{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}

{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen.Arbitrary.Generators where

import Gen.Helpers.StandardImports
import Gen.Arbitrary.Data

atype :: GG Type
dom   :: GG (Domainn Expr)
useFunc :: FuncsNames -> GG Bool


instance Default SS

instance Default Generators