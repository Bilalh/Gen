{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module TestGen.Prelude (
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
    , _ss
    , prettyBrackets
    , nub2
    , renderSized
    , noteFormat
    , rrError
    , logArr
) where

import TestGen.Helpers.StandardImports as X

import Common.Helpers as X

import TestGen.Arbitrary.Data as X
import {-# SOURCE #-} TestGen.Arbitrary.Generators as X

import TestGen.Helpers.Log as X (LogsTree(..), Pretty(..))
import TestGen.Helpers.OrderedType as X
import TestGen.Helpers.SizeOf as X

import TestGen.Helpers.Debug as X
import TestGen.Helpers.Helpers as X
import TestGen.Helpers.QuickCheck2 as X
import TestGen.Helpers.Standardise as X

import Data.Time
import Data.Time.Clock.POSIX(getPOSIXTime)

import System.Directory(getHomeDirectory, createDirectoryIfMissing)
import System.FilePath((</>), (<.>))

import qualified Data.Set as S


import qualified Control.Exception as C
import qualified Text.PrettyPrint as P

import qualified Data.Map as M

import TestGen.Helpers.TypeOf as X

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


rrError :: (HasLogger m)  => String -> [Doc] -> m a
rrError title docs = do
    lg <- getLog
    -- addLog "ggError" ["Last log"]
    error . show $ ( P.text $ padRight 15 ' ' title  )
        P.$+$ (nest 4 $ vcat (docs ))
        P.$+$ ""
#ifndef NO_GGERROR_LOGS
        P.$+$ nest 16 "==Logs=="
        P.$+$ (pretty (lg) )
#endif


ggError title docs = do
    lg <- getLog
    -- addLog "ggError" ["Last log"]
    st <- get
    error . show $ ( P.text $ padRight 15 ' ' title  )
        P.$+$ (nest 4 $ vcat (docs ++ [pretty st] ))
        P.$+$ ""
#ifndef NO_GGERROR_LOGS
        P.$+$ nest 16 "==Logs=="
        P.$+$ (pretty (lg) )
#endif

ggAssert :: Bool -> GG ()
ggAssert b = return $ C.assert b ()

-- for printing a name and a value
-- nn "dsd" <some Expr>
nn :: Pretty b => Doc -> b -> Doc
nn a b =  a <+> pretty b


_genlogs :: Pretty a =>  GG a -> SpecState -> IO ()
_genlogs e ss  = do
    res <-  (sample' (runStateT e (ss) ))
    forM_ res $ \(r,st) -> do
        putStrLn . renderSmall $ ""
            P.$+$ nest 16 "~~Result~~"
            P.$+$ ( pretty r )
            P.$+$ ""
            P.$+$ (nest 8 $ pretty st )
            P.$+$ ""
            P.$+$ nest 16 "==Logs=="
            P.$+$ nest 8 (pretty (logs_ st) )

_genfile :: Pretty a =>  GG a -> SpecState -> IO ()
_genfile e ss  = do

    c <- getCurrentTime
    let (y,m,d) = toGregorian $ utctDay c

    home <- getHomeDirectory
    date :: Integer<- round `fmap` getPOSIXTime
    let dir = home </> "__" </> "logs" </> "gen"
            </> ( intercalate "-" [show y, padShowInt 2 m, padShowInt 2 d] )
            </> (show date)
    createDirectoryIfMissing True dir

    res <-  (sample' (runStateT e (ss) ))


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


_gen :: Pretty a =>  GG a -> SpecState -> IO ()
_gen e ss  = do
    res <-  (sample' (runStateT e (ss) ))
    forM_ res $ \(r,_) -> do
        putStrLn . renderSmall $ ""
            P.$+$ ( pretty r )

renderSmall :: Pretty a => a -> String
renderSmall = P.renderStyle (P.style { P.lineLength = 120 }) . pretty

renderSized :: Pretty a => Int -> a -> String
renderSized n  = P.renderStyle (P.style { P.lineLength = n }) . pretty

noteFormat :: MonadIO m => Doc -> [Doc] -> m ()
noteFormat tx pr = liftIO . putStrLn . renderSized 120 $ hang tx 4 (vcat  pr)


_ss :: Depth -> SS
_ss d = def{depth_=d}


prettyBrackets :: Pretty a => a -> Doc
prettyBrackets = P.brackets . pretty

-- Might want to use a hash set at some point
-- nub is O(N^2) this is O(NlogN)
nub2 :: (Ord a, Hashable a) => [a] -> [a]
nub2 l = go S.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `S.member` s then go s xs
                                      else x : go (S.insert x s) xs

logArr :: (HasLogger m, Pretty a) => String -> [a] -> m [a]
logArr text arr = do
  addLog text [nn "arr" (prettyArr arr)]
  return arr
