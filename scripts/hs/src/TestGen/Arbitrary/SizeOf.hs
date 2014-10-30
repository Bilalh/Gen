{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestGen.Arbitrary.SizeOf where

import TestGen.Arbitrary.Helpers.Prelude
import qualified Data.Set as S


-- How many different unique values are in say a domain
class SizeOf a where
    sizeOf :: a -> Integer


instance SizeOf Domain where
    sizeOf DInt{..} = fromIntegral .  S.size .  S.fromList . concatMap rangeInts $ ranges
    sizeOf r = docError [
        "sizeOf not matched",
        pretty $ show r, pretty r
        ]



rangeInts :: Range Expr -> [Integer]
rangeInts (RSingle (ELit (EI a) ))     = [a]
rangeInts (RFromTo (ELit (EI a) )  (ELit (EI b) ) )  = [a..b]
rangeInts r = error . show $ vcat [
    "rangeInts not matched",
    pretty $ show r, pretty r
    ]
