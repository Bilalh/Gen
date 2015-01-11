{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TestGen.Reduce.Reduce_Debug where

import TestGen.Reduce.Data
import TestGen.Reduce.Runner
import TestGen.Reduce.Reduction
import TestGen.Reduce.UnusedDomains
import TestGen.Helpers.TypeOf

import TestGen.Prelude
import TestGen.Helpers.Runner(KindI(..), StatusI(..))
import TestGen.QCDebug(specE1)

import qualified TestGen.Arbitrary.Arbitrary as A
import qualified TestGen.Arbitrary.Domain as A
import qualified TestGen.Arbitrary.Expr as A

import qualified Data.Map as M
import qualified Test.QuickCheck as QC

import Control.Arrow((&&&))
import System.Random(randomIO)

import TestGen.Reduce.Reduce


_parts ::
        (ToEssence a1, FromEssence a) =>
        (a -> StateT EState Identity [a1]) -> E -> IO [a1]
_parts f e =
    case  fromEssence (e :: E) of
        Left er -> error . show .  (pretty &&& pretty . groom)  $ er
        Right ee -> do
            let spe   :: SpecE  = undefined
                seed            = 32
                state :: EState = EState{spec_=spe,sgen_=mkrGen seed,elogs_=LSEmpty}
                res             = runIdentity $ flip evalStateT state $ f ee
            mapM_ (print  . pretty . toEssence)  res
            return res

_partse :: ToEssence a =>
         (t -> StateT EState Identity [a]) -> t -> IO [a]
_partse f e = do
    let spe   :: SpecE  = undefined
        seed            = 32
        state :: EState = EState{spec_=spe,sgen_=mkrGen seed, elogs_=LSEmpty}
        res             = runIdentity $ flip evalStateT state $ f e
    mapM_ (print  . pretty . toEssence)  res
    return res

_partsf  = do
    let spe   :: SpecE  = undefined
        seed            = 32
        state :: EState = EState{spec_=spe,sgen_=mkrGen seed, elogs_=LSEmpty}
        res             = runIdentity  . flip evalStateT state
    res

_e :: FromEssence a => E -> a
_e e =  case fromEssence e of
        Left er -> error . show .  (pretty &&& pretty . groom) $ er
        Right ee -> ee

_k :: IO SpecE
_k = do
    -- let fp = "/Users/bilalh/CS/break_conjure/misc/1419393045_122/spec.specE"
    -- let fp = "/Users/bilalh/CS/break_conjure/2014-12-19_04-19_1418962766/RefineCompact_/rrErrorUnknown_/1418964459_41/spec.specE"
    -- let fp = "/Users/bilalh/CS/break_conjure/2014-12-19_04-19_1418962766/RefineCompact_/rrErrorUnknown_/1418965624_49/spec.specE"
    let fp = "/Users/bilalh/CS/break_conjure/2014-12-19_04-19_1418962766/Savilerow_/ParseError_/1418964183_16/spec.specE"
    -- let fp = "/Users/bilalh/CS/break_conjure/misc/1418964183_16_r/spec.specE"
    -- let fp = "/Users/bilalh/CS/break_conjure/out/1420607973_828/spec.specE"

    spe <- readSpecE fp
    reduceMain spe
           def{oErrKind_   = Savilerow_
              ,oErrStatus_ = ParseError_
              ,oErrEprime_ = Nothing
              ,outputDir_  = "/Users/bilalh/CS/break_conjure/out"
              ,rgen_       = mkrGen 3
              }


-- _tempRR :: RState
-- _tempRR  = def{oErrKind_   = Validate_
--               ,oErrEprime_ = Just "/Users/bilalh/CS/break_conjure/fixed/46c3d2b43f4e/2014-12-10_02-01_1418176894/_rrErrors/Validate_/rrErrorUnknown_/1418178864_89/model000001.eprime"
--               ,outputdir_  = "/Users/bilalh/CS/break_conjure/fixed/46c3d2b43f4e/2014-12-10_02-01_1418176894/_rrErrors/Validate_/rrErrorUnknown_/1418178864_89/reduce/"
--               ,rgen_       = mkrGen 6
--               }
-- hellod
