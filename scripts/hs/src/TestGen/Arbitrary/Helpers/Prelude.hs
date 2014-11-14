{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module TestGen.Arbitrary.Helpers.Prelude (
      module X
    , withDepth
    , withSameDepth
    , withDepthDec
    , withQuan
    , ggError
    , ggAssert
    , nn
    , _genlogs
    , _genfile
    , _gen
    , renderSmall
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

import Control.Monad.State.Strict as X (evalStateT,StateT)
import Control.Monad as X(filterM, guard)

import qualified Control.Exception as C
import qualified Text.PrettyPrint as P

import System.Directory(getHomeDirectory, createDirectoryIfMissing)
import System.FilePath((</>), (<.>))
import Data.Time.Clock.POSIX(getPOSIXTime)
import Data.Time

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

-- for printing a name and a value
-- nn "dsd" <some Expr>
nn :: Pretty b => Doc -> b -> Doc
nn a b =  a <+> pretty b


_genlogs :: Pretty a =>  GG a -> Int -> IO ()
_genlogs e n  = do
    res <-  (sample' (runStateT e (_ss n) ))
    forM_ res $ \(r,st) -> do
        putStrLn . renderSmall $ ""
            P.$+$ nest 16 "~~Result~~"
            P.$+$ ( pretty r )
            P.$+$ ""
            P.$+$ (nest 8 $ pretty st )
            P.$+$ ""
            P.$+$ nest 16 "==Logs=="
            P.$+$ nest 8 (pretty (logs_ st) )

_genfile :: Pretty a =>  GG a -> Int -> IO ()
_genfile e n  = do

    c <- getCurrentTime
    let (y,m,d) = toGregorian $ utctDay c

    home <- getHomeDirectory
    date <- round `fmap` getPOSIXTime
    let dir = home </> "__" </> "logs" </> "gen"
            </> ( intercalate "-" [show y, padShowInt 2 m, padShowInt 2 d] )
            </> (show date)
    createDirectoryIfMissing True dir

    res <-  (sample' (runStateT e (_ss n) ))


    forM_ (zip res [1..]) $ \((r,st),index :: Int) -> do

        putStrLn . renderSmall $ nest 75 (P.quotes $ pretty index )
            P.$+$ ( pretty r )

        writeFile (dir </> renderWide index <.> "output") $
            show $ ""
                P.$+$ ( pretty r )
                P.$+$ ""
                P.$+$ (nest 2 $ pretty st )
                P.$+$ ""
                P.$+$ nest 16 "==Logs=="
                P.$+$ nest 2 (pretty (logs_ st) )


_gen :: Pretty a =>  GG a -> Int -> IO ()
_gen e n  = do
    res <-  (sample' (runStateT e (_ss n) ))
    forM_ res $ \(r,st) -> do
        putStrLn . renderSmall $ ""
            P.$+$ ( pretty r )

renderSmall :: Pretty a => a -> String
renderSmall = P.renderStyle (P.style { P.lineLength = 74 }) . pretty
