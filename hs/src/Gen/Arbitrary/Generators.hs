
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen.Arbitrary.Generators(
      atype
    , dom
    , useFunc
    , Default(..)
    , useFunc_def
    ) where

import Gen.Prelude

import Gen.Arbitrary.Type(atype_def)
import Gen.Arbitrary.Domain(dom_def)
import qualified Data.Map as M


atype :: GG TType
atype = gets generators_ >>= \m -> gen_atype m

dom :: GG (Domainn Expr)
dom = gets generators_ >>= \m -> gen_dom m

useFunc :: FuncsNames -> GG Bool
useFunc fs = gets generators_ >>= \m -> return (  (gen_useFunc m) fs)


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
        { gen_atype   = atype_def
        , gen_dom     = dom_def
        , gen_useFunc = const True
        }

useFunc_def :: FuncsNames -> Bool
-- useFunc_def AtoInt = True
-- useFunc_def Aparts = True
-- useFunc_def Amin   = True
useFunc_def Ahist  = True
useFunc_def _      = True
