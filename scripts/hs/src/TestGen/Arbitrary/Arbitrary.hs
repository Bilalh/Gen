{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Arbitrary where

import AST.Imports
import TestGen.Arbitrary.Data
import TestGen.Arbitrary.Domain
import TestGen.Arbitrary.Expr
import TestGen.Arbitrary.Helpers

import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Map as M

instance Arbitrary SpecE where
    arbitrary = sized spec

spec :: Depth -> Gen SpecE
spec depth = do
    doms <- listOfB 1 (min (depth*2) 10) (dom depth)
    let withNames =  zipWith (\d i -> (name i , Find d)) doms [1 :: Int ..]
    let mappings  = M.fromList withNames

    let state = SS{depth_=depth, doms_=mappings, nextNum_ = length doms + 1,newVars_=[]}

    exprs <- listOfB 0 15 ( expr state)

    return $ SpecE mappings exprs

    where name i =  T.pack $  "var" ++  (show  i)
