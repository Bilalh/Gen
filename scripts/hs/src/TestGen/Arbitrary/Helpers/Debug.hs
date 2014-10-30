{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module TestGen.Arbitrary.Helpers.Debug(
    docError, tracer, tracet, tracef
    )
    where

import Language.E hiding(trace)

import qualified Data.HashSet as S

#ifdef TTRACE
import Debug.Trace(trace)
#endif

docError :: [Doc] -> a
docError = error . show . vcat

suppress :: S.HashSet String
suppress = S.fromList [
          "exprOf"
        , "atype"
    ]

only :: S.HashSet String
only = S.fromList [
        ""
    ]

#ifdef TTRACE

tracer :: String -> [Doc] -> a -> a
tracer title _   a | title `S.member` suppress = a
-- tracer title docs a | not (title `S.member` only) =  a

tracer title docs a =  trace
        ( show  $  (" ¦" <+> pretty (padRight 15 ' ' title)) <+> (nest 4 $ vcat docs)  ) a

tracet :: String -> [Doc] -> Bool
tracet title docs =  tracer title docs True
tracef :: String -> [Doc] -> Bool
tracef title docs =  tracer title docs False

#else

tracer :: String -> [Doc] -> a -> a
tracer _ _ a = a

tracet :: String -> [Doc] -> Bool
tracet _ _  = True
tracef :: String -> [Doc] -> Bool
tracef _ _ =  False

#endif
