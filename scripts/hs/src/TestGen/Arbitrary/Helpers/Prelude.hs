{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module TestGen.Arbitrary.Helpers.Prelude (
      module X
    , withDepth
    , withSameDepth
    , withDepthDec
    , withQuan
    , ggError
    , ggAssert
) where

import AST.Imports as X


import Test.QuickCheck as X hiding (maxSize)

import TestGen.Arbitrary.Data as X
import TestGen.Arbitrary.Helpers.Debug as X
import TestGen.Arbitrary.Helpers.Helpers as X
import TestGen.Arbitrary.Helpers.QuickCheck2 as X
import TestGen.Arbitrary.SizeOf as X
import TestGen.Arbitrary.Helpers.Log as X (LogsTree(..), Pretty(..))
import Common.Helpers as X

import Data.Set as X (Set)
import Development.Placeholders as X (placeholder,notImplemented,todo)
import Language.E as X
import Text.Groom as X (groom)

import Control.Monad.State.Strict as X (evalStateT)
import Control.Monad as X(filterM)

import qualified Control.Exception as C
import qualified Text.PrettyPrint as P


withDepth :: Depth -> GG a -> GG a
withDepth newDepth f = do
    oldDepth <- gets depth_
    addLog "withDepth" ["from" <+> pretty oldDepth, "to" <+> pretty newDepth ]

    modify $ \st -> st{ depth_ = newDepth }
    res <- f
    modify $ \st -> st{ depth_ = oldDepth }
    addLog "withDepth" ["from" <+> pretty newDepth, "backto" <+> pretty oldDepth ]
    return res

withSameDepth :: GG a -> GG a
withSameDepth f = do
    oldDepth <- gets depth_
    addLog "withSameDepth" ["from" <+> pretty oldDepth ]
    res <- f
    modify $ \st -> st{ depth_ = oldDepth }
    addLog "withSameDepth" ["backto" <+> pretty oldDepth ]
    return res

withDepthDec :: GG a -> GG a
withDepthDec f = do
    oldDepth <- gets depth_
    addLog "withDepthDec" ["from" <+> pretty oldDepth <+> "to" <+> pretty(oldDepth - 1) ]

    modify $ \st -> st{ depth_ = oldDepth - 1 }
    res <- f
    modify $ \st -> st{ depth_ = oldDepth }
    addLog "withDepthDec" ["from" <+> pretty (oldDepth - 1) <+> "backto" <+> pretty oldDepth ]
    return res



-- Use so that variables in qualifiers don't get reused outside
withQuan  :: GG a -> GG a
withQuan f = do
    d <- gets depth_
    addLog "withQuan" ["depth_" <+> pretty d ]
    old <- gets newVars_
    res <- f
    modify $ \st -> st{ newVars_ = old }
    d' <- gets depth_
    addLog "withQuan" ["end, depth_" <+> pretty d' ]
    return res


ggError :: String -> [Doc] -> GG a
ggError title docs = do
    -- addLog "ggError" ["Last log"]
    st <- get
    error . show $ ( P.text $ padRight 15 ' ' title  )
        P.$+$ (nest 4 $ vcat (docs ++ [pretty st] ))
        P.$+$ ""
        P.$+$ nest 16 "==Logs=="
        P.$+$ (pretty (logs_ st) )

ggAssert :: Bool -> GG ()
ggAssert b = return $ C.assert b ()
