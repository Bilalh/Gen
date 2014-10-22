{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestGen.Arbitrary.SizeOf where

import AST.Imports
import qualified Data.Set as S
import Language.E


-- How many different unique values are in say a domain
class SizeOf a where
    sizeOf :: a -> Integer


instance SizeOf Domain where
    sizeOf DInt{..} = fromIntegral .  S.size .  S.fromList . concatMap rangeInts $ ranges
    sizeOf r = error . show $ vcat [
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
