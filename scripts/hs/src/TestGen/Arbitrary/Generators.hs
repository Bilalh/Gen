{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module TestGen.Arbitrary.Generators where

import TestGen.Prelude

import TestGen.Arbitrary.Type(atype_def)
import TestGen.Arbitrary.Domain(dom_def)
import qualified Data.Map as M


atype :: GG Type 
atype = gets generators_ >>= \m -> gen_atype m

dom :: GG Domain
dom = gets generators_ >>= \m -> gen_dom m

instance Default SS where
     def = SS
             { depth_      = error "no depth"
             , doms_       = M.empty
             , nextNum_    = 1
             , newVars_    = []
             , logs_       = LSEmpty
             , __lc        = 0
             , beConstant_ = False
             , generators_ = def
             }


instance Default Generators where
    def = Generators
        { gen_atype = atype_def
        , gen_dom   = dom_def
        }