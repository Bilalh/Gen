{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module TestGen.Arbitrary.Generators where

import TestGen.Arbitrary.Data
import Data.Default as X ( Default, def )

atype :: GG Type 


instance Default SS

instance Default Generators