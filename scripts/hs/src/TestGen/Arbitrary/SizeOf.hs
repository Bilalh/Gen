{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestGen.Arbitrary.SizeOf where

import AST.Imports
import TestGen.Arbitrary.Data
import TestGen.Helpers.Debug
import qualified Data.Set as S


-- How many different unique values are in say a domain
class SizeOf a where
    sizeOf :: a -> Integer

class DepthOf a where
    depthOf :: a -> Integer


instance SizeOf Domain where
    sizeOf DInt{..} = fromIntegral .  S.size .  S.fromList . concatMap rangeInts $ ranges
    sizeOf r = docError [
        "sizeOf not matched",
        pretty $ show r, pretty r
        ]


instance DepthOf Type where
    depthOf TInt  = 0
    depthOf TBool = 0

    depthOf (TMatix inner)   = 1 + depthOf inner
    depthOf (TSet inner)     = 1 + depthOf inner
    depthOf (TMSet inner)    = 1 + depthOf inner
    depthOf (TFunc from to ) = 1 + nonEmpty (maximum . map depthOf) [from, to]
    depthOf (TTuple inners ) = 1 + nonEmpty (maximum . map depthOf) inners
    depthOf (TRel inners )   = 2 + nonEmpty (maximum . map depthOf) inners
    depthOf (TPar inner)     = 1 + depthOf inner

    --FIXME what should the depth of any be?
    depthOf TAny             = 0

    depthOf ty = docError [ "depthOf not implemented", pretty ty ]

nonEmpty :: ([t] -> Integer) -> [t] -> Integer
nonEmpty _ [] = 0
nonEmpty f xs = f xs

rangeInts :: Range Expr -> [Integer]
rangeInts (RSingle (ELit (EI a) ))     = [a]
rangeInts (RFromTo (ELit (EI a) )  (ELit (EI b) ) )  = [a..b]
rangeInts r = docError [
    "rangeInts not matched",
    pretty $ show r, pretty r
    ]
