{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Arbitrary where

import TestGen.Prelude
import TestGen.Arbitrary.Data
import TestGen.Arbitrary.Domain
import TestGen.Arbitrary.Expr

import qualified Data.Map as M
import qualified Data.Text as T
import Data.List

data WithLogs a = WithLogs a LogsTree

instance Arbitrary (WithLogs SpecE) where
    arbitrary = do
        (specE, logs) <- sized spec'
        return $ WithLogs specE logs

    shrink (WithLogs (SpecE ds es) a) = 
        let sps = map (\x -> WithLogs (SpecE ds x) a ) (tails2 es)
        in sps

instance Arbitrary SpecE where
    arbitrary = sized spec
    shrink (SpecE ds es) = 
        let sps = map (SpecE ds) (tails2 es)
        in sps


tails2 :: [a] -> [[a]]
tails2 [] = []
tails2 xs = tail (tails xs)

instance Show (WithLogs SpecE) where
    show (WithLogs specE logs) =
        "WithLogs ( " ++ show specE ++ " ) "

spec :: Depth -> Gen SpecE
spec depth =  do
    (specE, _) <- spec' depth
    return specE

spec' :: Depth -> Gen (SpecE, LogsTree) 
spec' = flip spec'' def


spec'' :: Depth -> Generators -> Gen (SpecE, LogsTree) 
spec'' depth gens  = do

    let state = def{depth_= depth `div` 2, generators_=gens}
    (doms,state') <- runStateT  ( listOfBounds (1, (min (depth*2) 10)) dom) state

    let withNames =  zipWith (\d i -> (name i , Find d)) doms [1 :: Int ..]
    let mappings  = M.fromList withNames


    let state'' =  state'{doms_=mappings, depth_ =depth, nextNum_ = length doms + 1}
    (exprs,sfinal) <- runStateT (listOfBounds (0,15) expr) state''

    return $ (SpecE mappings exprs, logs_ sfinal)

    where name i =  T.pack $  "var" ++  (show  i)
