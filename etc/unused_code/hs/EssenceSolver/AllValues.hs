
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module EssenceSolver.AllValues(allValues) where

import Common.Helpers(mkInt, getInt)
import EssenceSolver.Checker(eguard,domSizeC)

import Language.E

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
        allDoms

    where
        mkMatrix es = [xMake| value.matrix.values := es
                            | value.matrix.indexrange := [irDom] |]

-- Function
allValues
    [xMatch| attrs       := domain.function.attributes.attrCollection
           | [innerFrom] := domain.function.innerFrom
           | [innerTo]   := domain.function.innerTo |] =
    let
        valFrom  = allValues innerFrom
        valTo    = allValues innerTo

        sFrom    = domSizeC innerFrom
        sTo      = domSizeC innerTo

        tos size = replicateM size valTo
        gen fr   = map (zipWith mkMapping fr) (tos $ length fr)
        maps     =
           -- trace (show $ map length $ subsequences valFrom) $
           concatMap gen (subsequences valFrom)

        allFuncs =   map mkFunction maps

        attrMeta :: (Text, Maybe E) -> E -> Bool
        attrMeta ("size",Just v) e  =
            eguard [eMake| |defined(&e)| = &v  |]

        attrMeta ("minSize",Just v) e  =
            eguard [eMake| |defined(&e)| >= &v  |]

        attrMeta ("maxSize",Just v) e  =
            eguard [eMake| |defined(&e)| <= &v  |]

        attrMeta ("total",Nothing) e  =
            eguard [eMake| |defined(&e)| = &sFrom  |]

        attrMeta a e = error . show $ vcat [ "attrMeta", pretty a, pretty e ]

        filters  =  combinedFilter $ map (attrMeta . getAttr) attrs
    in
        filters allFuncs

    where

        mkMapping from to = [xMake| mapping := [from,to] |]
        mkFunction es = [xMake| value.function.values := es |]


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
