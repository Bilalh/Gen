{-# LANGUAGE CPP #-}
module Gen.Arbitrary.Prelude (
      module X
    , withDepth
    , withSameDepth
    , withDepthDec
    , withQuan
    , ggError
    , ggAssert
    , _genlogs
    , _genfile
    , _genSample
    , prettyBrackets
    , logArr
    , genSample
) where

import {-# SOURCE #-} Gen.Arbitrary.Generators as X

import Gen.Arbitrary.Data          as X
import Gen.Arbitrary.Debug           as X
import Gen.Helpers.Log             as X (LogsTree (..),nullLogs)
import Gen.Arbitrary.OrderedType     as X
import Gen.Arbitrary.QuickCheck2     as X
import Gen.Helpers.SizeOf          as X
import Gen.Imports as X
import Gen.Helpers.TypeOf          as X

import Data.Time
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory      (getHomeDirectory)
import Test.QuickCheck( generate, sample')

import qualified Control.Exception as C
import qualified Text.PrettyPrint  as P

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




ggError ::  (HasLogger m, Pretty a, MonadState a m) => String -> [Doc] -> m b
ggError title docs = do
    lg <- getLog
    addLog "ggError" ["Last log"]
    st  <- get
    error . show $ ( P.text $ padRight 15 ' ' title  )
        P.$+$ (nest 4 $ vcat (docs ++ [pretty st] ))
        P.$+$ ""
#ifndef NO_GGERROR_LOGS
        P.$+$ nest 16 "==Logs=="
        P.$+$ (pretty (lg) )
#endif


ggAssert :: Bool -> GG ()
ggAssert b = return $ C.assert b ()



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

        writeFile (dir </> renderSized 120 index <.> "output") $
            show $ ""
                P.$+$ ( pretty r )
                P.$+$ ""
                P.$+$ (nest 2 $ pretty st )
                P.$+$ ""
                P.$+$ nest 16 "==Logs=="
                P.$+$ nest 2 (pretty (logs_ st) )


_genSample :: Pretty a =>  GG a -> SpecState -> IO ()
_genSample e ss  = do
    res <-  (sample' (runStateT e (ss) ))
    forM_ res $ \(r,_) -> do
        putStrLn . renderSmall $ ""
            P.$+$ ( pretty r )

genSample :: GG a -> SpecState -> IO [a]
genSample e ss  = do
   mapM (\_ -> generate $ evalStateT  e ss ) [0..10 :: Int]


prettyBrackets :: Pretty a => a -> Doc
prettyBrackets = P.brackets . pretty


logArr :: (HasLogger m, Pretty a) => String -> [a] -> m [a]
logArr text arr = do
  addLog text [nn "arr" (prettyArr arr)]
  return arr
