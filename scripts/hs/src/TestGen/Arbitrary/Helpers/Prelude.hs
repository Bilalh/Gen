{-# LANGUAGE OverloadedStrings #-}
module TestGen.Arbitrary.Helpers.Prelude (
      module X
    , withDepth
    , withDepthDec
    , ggError
    , ggAssert
) where

import AST.Imports as X

import TestGen.Arbitrary.Data as X
import TestGen.Arbitrary.Helpers.QuickCheck2 as X
import TestGen.Arbitrary.Helpers.Debug as X
import TestGen.Arbitrary.Helpers.Helpers as X
import Test.QuickCheck as X hiding (maxSize)

import Language.E as X
import Text.Groom as X (groom)
import Data.Set as X (Set)

import Development.Placeholders as X (placeholder,notImplemented)

import qualified Text.PrettyPrint as P

import Control.Exception as X(assert)


withDepth :: Depth -> GG a -> GG a
withDepth newDepth f = do
    oldDepth <- gets depth_
    modify $ \st -> st{ depth_= newDepth }
    res <- f
    modify $ \st -> st{ depth_= oldDepth }
    return res

withDepthDec :: GG a -> GG a
withDepthDec f = do
    oldDepth <- gets depth_
    modify $ \st -> st{ depth_= oldDepth - 1 }
    res <- f
    modify $ \st -> st{ depth_= oldDepth }
    return res

ggError :: String -> [Doc] -> GG a
ggError title docs = do
    addLog "ggError" ["Last log"]
    st <- get
    error . show $ ( P.text $ padRight 15 ' ' title  )
        P.$+$ (nest 4 $ vcat (docs ++ [pretty st] ))
        P.$+$ nest 4 "--Logs--"
        P.$+$ (nest 4 $ pretty (logs_ st) )

ggAssert :: Bool -> GG ()
ggAssert b = return $ assert b ()
