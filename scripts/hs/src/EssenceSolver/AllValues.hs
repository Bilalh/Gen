{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module EssenceSolver.AllValues(allValues) where

import Common.Helpers(mkInt, getInt)
import EssenceSolver.Checker(eguard,domSizeC)

-- import Language.E
import Language.E hiding (trace)
import Debug.Trace(trace)

allValues :: E -> [E]

--int
allValues
    [xMatch| rs := domain.int.ranges |] =
    concatMap getIntVals rs

    where
        getIntVals [xMatch| [Prim (I j)] := range.single.value.literal |] =
            [mkInt j]
        getIntVals [xMatch| [Prim (I l), Prim(I u)] := range.fromTo.value.literal |] =
            map mkInt [l..u]
        getIntVals [xMatch| [Prim (I _)] := range.from.value.literal |]  =
            error "int unbounded"
        getIntVals _ = error "getIntVals"


--set
allValues
    [xMatch| attrs      := domain.set.attributes.attrCollection
           | [innerDom] := domain.set.inner |] =
    let
        allInners = allValues innerDom
        allSets   = map mkSet . subsequences $ allInners
        filters   = combinedFilter $ map (attrMeta . getAttr) attrs
    in
        filters allSets

    where
        f a e = attrMeta a e || attrSize a e
        mkSet es =  [xMake| value.set.values := es |]

        attrMeta :: (Text, Maybe E) -> E -> Bool
        attrMeta ("size",Just v) e  =
            eguard [eMake| |&e| = &v  |]

        attrMeta ("minSize",Just v) e  =
            eguard [eMake| |&e| >= &v  |]

        attrMeta ("maxSize",Just v) e  =
            eguard [eMake| |&e| <= &v  |]

        attrMeta a e = error . show $ vcat [ "attrMeta", pretty a, pretty e ]

-- Tuple
allValues
    [xMatch| innerDoms := domain.tuple.inners|] =
    let
        allInners = map allValues innerDoms
        allDoms   = map mkTuple . sequence $ allInners
    in
       allDoms

    where
        mkTuple es =  [xMake|  value.tuple.values := es |]

-- Matrix
allValues
    [dMatch| matrix indexed by [&irDom] of &innerDom  |] =
    let
        dsize     = fromInteger . getInt $ domSizeC irDom
        allInners = allValues innerDom
        allDoms   = map mkMatrix .  replicateM dsize $ allInners
    in
        -- trace (show . vcat $ map pretty allDoms) allDoms
        trace (show . vcat $ map pretty allDoms) allDoms

    where
        mkMatrix es = [xMake| value.matrix.values := es
                            | value.matrix.indexrange := [irDom] |]


allValues e = error . show $ vcat  [ "Missing case in AllValues", pretty e, prettyAsTree e ]


combinedFilter :: [a -> Bool] -> [a] -> [a]
combinedFilter fs =  filter (\a -> all (\f -> f a )  fs  )

combinedFilter2 :: [a -> Bool] -> [a] -> [a]
combinedFilter2 fs =  _compose (map filter fs)

    where
    -- Magically combine a list functions
    _compose :: [a -> a] -> (a -> a)
    _compose = foldl (flip (.)) id


getAttr :: E -> (Text, Maybe E)
getAttr [xMatch| [Prim (S n)] := attribute.nameValue.name.reference
               |          [v] := attribute.nameValue.value
               |] = (n,Just v)

getAttr [xMatch| [Prim (S n)] := attribute.name.reference
               |] = (n,Nothing)

getAttr e = error . show  $ vcat [ "getAttr", pretty e, prettyAsTree e]


attrSize :: (Text, Maybe E) -> E -> Bool
attrSize ("size",Just v) e  =
    eguard [eMake| |&e| = &v  |]

attrSize ("minSize",Just v) e  =
    eguard [eMake| |&e| >= &v  |]

attrSize ("maxSize",Just v) e  =
    eguard [eMake| |&e| <= &v  |]

attrSize _ _ = True
