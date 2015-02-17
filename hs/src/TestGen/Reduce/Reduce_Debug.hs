{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module TestGen.Reduce.Reduce_Debug where

import TestGen.Reduce.Data
import TestGen.Reduce.Runner
import TestGen.Reduce.Reduction
import TestGen.Reduce.UnusedDomains
import TestGen.Helpers.TypeOf

import TestGen.Prelude
import TestGen.Helpers.Runner(KindI(..), StatusI(..))

import qualified TestGen.Arbitrary.Arbitrary as A
import qualified TestGen.Arbitrary.Domain as A
import qualified TestGen.Arbitrary.Expr as A

import qualified Data.Map as M
import qualified Test.QuickCheck as QC

import Control.Arrow((&&&))
import System.Random(randomIO)

import TestGen.Reduce.Reduce


-- _parts ::
--         (ToEssence a1, FromEssence a) =>
--         (a -> StateT EState Identity [a1]) -> E -> IO [a1]
-- _parts f e =
--     case  fromEssence (e :: E) of
--         Left er -> error . show .  (pretty &&& pretty . groom)  $ er
--         Right ee -> do
--             let spe   :: Spec  = undefined
--                 seed            = 32
--                 state :: EState = EState{spec_=spe,sgen_=mkrGen seed,elogs_=LSEmpty}
--                 res             = runIdentity $ flip evalStateT state $ f ee
--             mapM_ (print  . pretty . toEssence)  res
--             return res

-- _partse :: ToEssence a =>
--          (t -> StateT EState Identity [a]) -> t -> IO [a]
-- _partse f e = do
--     let spe   :: Spec  = undefined
--         seed            = 32
--         state :: EState = EState{spec_=spe,sgen_=mkrGen seed, elogs_=LSEmpty}
--         res             = runIdentity $ flip evalStateT state $ f e
--     mapM_ (print  . pretty . toEssence)  res
--     return res

-- _partsf  = do
--     let spe   :: Spec  = undefined
--         seed            = 32
--         state :: EState = EState{spec_=spe,sgen_=mkrGen seed, elogs_=LSEmpty}
--         res             = runIdentity  . flip evalStateT state
--     res

-- _e :: FromEssence a => E -> a
-- _e e =  case fromEssence e of
--         Left er -> error . show .  (pretty &&& pretty . groom) $ er
--         Right ee -> ee

_k :: FilePath -> FilePath -> IO (Spec, RState)
_k out inn= do
  reduceMain def{oErrKind_=KindAny_, oErrStatus_=StatusAny_, cores_=1,newConjure_=True,oErrEprime_=Nothing ,rgen_= mkrGen 4, outputDir_=out, specDir_=inn}

plogs :: (a,RState) -> Doc
plogs (_,RState{rlogs_}) = pretty rlogs_


-- _k :: IO Spec
-- _k = do
--     -- let fp = "/Users/bilalh/CS/break_conjure/misc/1419393045_122/spec.specE"
--     -- let fp = "/Users/bilalh/CS/break_conjure/2014-12-19_04-19_1418962766/RefineCompact_/rrErrorUnknown_/1418964459_41/spec.specE"
--     -- let fp = "/Users/bilalh/CS/break_conjure/2014-12-19_04-19_1418962766/RefineCompact_/rrErrorUnknown_/1418965624_49/spec.specE"
--     let fp = "/Users/bilalh/CS/break_conjure/2014-12-19_04-19_1418962766/Savilerow_/ParseError_/1418964183_16/spec.specE"
--     -- let fp = "/Users/bilalh/CS/break_conjure/misc/1418964183_16_r/spec.specE"
--     -- let fp = "/Users/bilalh/CS/break_conjure/out/1420607973_828/spec.specE"

--     spe <- readSpecE fp
--     reduceMain1 spe
--            def{oErrKind_   = Savilerow_
--               ,oErrStatus_ = ParseError_
--               ,oErrEprime_ = Nothing
--               ,outputDir_  = "/Users/bilalh/CS/break_conjure/out"
--               ,rgen_       = mkrGen 3
--               }


-- _j :: IO Spec
-- _j = do
--   let fp = "/Users/bilalh/CS/break_conjure/2014-12-19_04-19_1418962766/RefineCompact_/ErrorUnknown_/1418964459_41"

--   reduceMain def{
--        oErrKind_   = RefineRandom_
--      , oErrStatus_ = ErrorUnknown_
--      , outputDir_  = "/Users/bilalh/CS/break_conjure/2014-12-19_04-19_1418962766/RefineCompact_/ErrorUnknown_/1418964459_41/out"
--      , rgen_       = mkrGen 4
--      , cores_      = 4
--      , oErrEprime_ = Nothing
--      , specDir_    = fp
--      , newConjure_ = True
--      }
