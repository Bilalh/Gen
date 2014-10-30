{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Arbitrary where

import TestGen.Arbitrary.Helpers.Prelude
import TestGen.Arbitrary.Data
import TestGen.Arbitrary.Domain
import TestGen.Arbitrary.Expr

import qualified Data.Map as M
import qualified Data.Text as T


instance Arbitrary SpecE where
    arbitrary = sized spec

spec :: Depth -> Gen SpecE
spec depth = do

    doms <- listOfB 1 (min (depth*2) 10) (dom depth)
    -- doms <- vectorOf 2  (dom depth)
    let withNames =  zipWith (\d i -> (name i , Find d)) doms [1 :: Int ..]
    let mappings  = M.fromList withNames

    let state = (_ss depth){doms_=mappings, nextNum_ = length doms + 1}

    (exprs,ss) <- runStateT (listOfBounds (0,15) expr) state

    return $ SpecE mappings exprs

    where name i =  T.pack $  "var" ++  (show  i)
