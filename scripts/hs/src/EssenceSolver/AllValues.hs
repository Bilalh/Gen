{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module EssenceSolver.AllValues(allValues) where

import Common.Helpers(mkInt, getIntMaybe)
import EssenceSolver.Checker(eguard)

import Language.E

allValues :: E -> [E]
allValues [xMatch| rs := domain.int.ranges |] =
    concatMap getIntVals rs

    where
        getIntVals [xMatch| [Prim (I j)] := range.single.value.literal |] =
            [mkInt j]
        getIntVals [xMatch| [Prim (I l), Prim(I u)] := range.fromTo.value.literal |] =
            map mkInt [l..u]
        getIntVals [xMatch| [Prim (I _)] := range.from.value.literal |]  =
            error "int unbounded"
        getIntVals _ = error "getIntVals"

allValues [xMatch| attrs      := domain.set.attributes.attrCollection
                 | [innerDom] := domain.set.inner |] =
    let
        allInners = allValues innerDom
        allSets   = map mkSet . subsequences $ allInners
        -- filters   = compose (map (attrMeta . getAttr ) attrs)
        filters  = combinedFilter (map (attrMeta . getAttr) attrs)
    in
        filters allSets

    where
        mkSet es =  [xMake| value.set.values := es |]

        attrMeta :: (Text, Maybe E) -> E -> Bool
        attrMeta ("size",Just v) e  =
            eguard [eMake| |&e| = &v  |]

        reduceDomain :: [E] -> (Text, Maybe E) -> [E]
        reduceDomain es attr = es

allValues e = error . show $ vcat  [ "Missing case in AllValues", pretty e, prettyAsTree e ]

combinedFilter :: [a -> Bool] -> [a] -> [a]
combinedFilter fs =  filter (\a -> all (\f -> f a )  fs  )


getAttr :: E -> (Text, Maybe E)
getAttr [xMatch| [Prim (S n)] := attribute.nameValue.name.reference
               |          [v] := attribute.nameValue.value
               |] = (n,Just v)

getAttr [xMatch| [Prim (S n)] := attribute.name.reference
               |] = (n,Nothing)

getAttr e = error . show  $ vcat [ "getAttr", pretty e, prettyAsTree e]

-- Magically combine a list functions
compose :: [a -> a] -> (a -> a)
compose fs v = foldl (flip (.)) id fs $ v
