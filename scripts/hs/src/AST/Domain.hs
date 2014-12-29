{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module AST.Domain where

import AST.FromEssence(FromEssence(..))
import AST.ToEssence(ToEssence(..))
import AST.Types
import AST.Helper
import {-# SOURCE #-} AST.Range

import Language.E

import GHC.Generics
import Data.Typeable

--  We have two kinds of attrs
--  size       :: Maybe Integer
--  surjective :: Bool
--
--  It might be nice to able to act uniformly upon them
-- e.g
-- data Atrr = Val Integer
--           | Has Bool
--           | Void
-- Or we could have a hasAttr typeclass
-- so if have a list of attrs we filter the ones that are active

--data TestA_ = TestA_ Expr deriving(Show)
--_testb (TestA_ expr)  = toEssence expr

dint, dset, dmset, dmat, dfunc, drel, dpar, dtuple  :: Domain
dint  = DInt{ranges = undefined}
dset  = DSet{inner = undefined, size = Nothing
            , minSize = Nothing, maxSize = Nothing}
dmset = DMSet{inner = undefined, size = Nothing, minSize = Nothing
            , maxSize = Nothing
            , maxOccur = Nothing, minOccur = Nothing  }
dmat  = DMat{innerIdx=undefined, inner=undefined}
dfunc = DFunc{size = Nothing, minSize = Nothing, maxSize = Nothing
             ,innerFrom=undefined, innerTo=undefined
             ,injective=False, surjective=False, total=False}
drel = DRel{inners=undefined, size=Nothing, minSize=Nothing,maxSize=Nothing
           ,reflexive=False, symmetric=False}
dpar = DPar{inner=undefined, size=Nothing, maxSize=Nothing, minSize=Nothing
           ,numParts=Nothing, maxNumParts=Nothing, minNumParts=Nothing
           ,partSize=Nothing, maxPartSize=Nothing, minPartSize=Nothing
           ,regular=False, complete=False }
dtuple = DTuple{inners=undefined}

dintRange :: Integer -> Integer -> Domain
dintRange lower upper = dint{ranges= [RFromTo (ELit $ EI lower) (ELit $ EI upper)]}



instance ToEssence Domain where
    toEssence DInt{..} =
        [xMake| domain.int.ranges := map toEssence ranges |]

    toEssence DBool = [dMake| bool |]

    toEssence DSet{..} =
        let e = [dMake| set of &domE |] in
        addAttrs (map mkAttr $ filter ( isJust . snd )   attrs ) e

        where
            domE = toEssence inner
            attrs = sortBy (comparing fst)
                $ [ ("size", size), ("minSize", minSize),  ("maxSize", maxSize) ]


    toEssence DMSet{..} =
        let e = [dMake| mset of &domE |] in
        addAttrs (map mkAttr $ filter ( isJust . snd ) attrs ) e

        where
            domE = toEssence inner
            attrs = sortBy (comparing fst)
                $ [ ("size", size), ("minSize", minSize),  ("maxSize", maxSize),
                    ("minOccur", minOccur), ("maxOccur", maxOccur)  ]

    toEssence DFunc{..} =
        let e = [dMake| function &dom1 --> &dom2 |] in
        addAttrs ( combineAttrs attrs attrs' ) e
        where
            dom1 = toEssence innerFrom
            dom2 = toEssence innerTo

            attrs = sortBy (comparing fst)
                $ [ ("size", size), ("minSize", minSize),  ("maxSize", maxSize) ]
            attrs' = sortBy (comparing fst)
                $ [("injective",injective),  ("surjective", surjective)
                , ("total",total) ]

    toEssence DRel{..} = addAttrs (combineAttrs attrs attrs') e where
        e = [xMake| domain.relation.attributes.attrCollection := []
                  | domain.relation.inners := is |]
        is = map toEssence inners

        attrs = sortBy (comparing fst)
            $ [("size",size), ("minSize", minSize),  ("maxSize", maxSize) ]
        attrs' = sortBy (comparing fst)
            $ [("reflexive",reflexive), ("symmetric", symmetric) ]

    toEssence DPar{..} = addAttrs (combineAttrs attrs attrs') e where
        e = [xMake| domain.partition.attributes.attrCollection := []
                  | domain.partition.inner := [is] |]
        is = toEssence inner

        attrs = sortBy (comparing fst)
            $ [("size",size), ("minSize", minSize),  ("maxSize", maxSize)
              , ("numParts", numParts), ("maxNumParts", maxNumParts)
              , ("minNumParts", maxNumParts), ("partSize", partSize)
              , ("maxPartSize", maxPartSize), ("minPartSize", minPartSize)]
        attrs' = sortBy (comparing fst)
            $ [("regular",regular), ("complete", complete) ]

    toEssence DMat{..} = [dMake| matrix indexed by [&ix] of &inE |]
        where
            ix  = toEssence innerIdx
            inE = toEssence inner

    toEssence DTuple{..} = [xMake| domain.tuple.inners := vs |]
        where
        vs = map toEssence inners

instance Pretty Domain where

    pretty p = pretty $ toEssence p

instance FromEssence Domain where
    -- integers
    fromEssence [xMatch| r := domain.int.ranges |] = DInt <$> mapM fromEssence r

    -- sets
    fromEssence [xMatch| [i]  := domain.set.inner
                       | [as] := domain.set.attributes |] = do
        iv <- fromEssence i
        let [sv,mx,mn] = map (fetchAttrValue as) ["size","maxSize","minSize"]
        return dset{inner=iv,size=sv,maxSize=mx,minSize=mn}

    -- multi-set
    fromEssence [xMatch| [i]  := domain.mset.inner
                       | [as] := domain.mset.attributes |] = do
        iv <- fromEssence i
        let [sv,mx,mn,mxo,mno] = map (fetchAttrValue as)
                ["size","maxSize","minSize","maxOccur","minOccur"]
        return DMSet{inner=iv,size=sv,maxSize=mx,minSize=mn,maxOccur=mxo,minOccur=mno}

    -- function
    fromEssence [xMatch| [ifrom] := domain.function.innerFrom
                       | [ito]   := domain.function.innerTo
                       | [as]    := domain.function.attributes |] = do
        ifv <- fromEssence ifrom
        itv <- fromEssence ito
        let [sv,mx,mn] = map (fetchAttrValue as) ["size","maxSize","minSize"]
            [s,i,t]    = map (fetchAttr as) ["surjective","injective","total"]
        return DFunc{innerFrom=ifv,innerTo=itv,size=sv,maxSize=mx,minSize=mn,surjective=s,injective=i,total=t}

    -- Partition
    fromEssence [xMatch| [i]  := domain.partition.inner
                       | [as] := domain.partition.attributes |] = do
        iv <- fromEssence i
        let [sv,mx,mn,np,mxp,mnp,ps,mxps,mnps] = map (fetchAttrValue as)
                ["size","maxSize","m,inSize","numParts","maxNumParts","minNumParts"
                ,"partSize","maxPartSize","minPartSize"]
            [r,c]    = map (fetchAttr as) ["regular","complete"]
        return DPar{inner=iv,size=sv,maxSize=mx,minSize=mn,numParts=np,maxNumParts=mxp,minNumParts=mnp
                ,partSize=ps,minPartSize=mnps,maxPartSize=mxps,regular=r,complete=c}

    -- Relation
    fromEssence [xMatch| is   := domain.relation.inners
                       | [as] := domain.relation.attributes |] = do
        iv <- mapM fromEssence is
        let [sv,mx,mn] = map (fetchAttrValue as) ["size","maxSize","minSize"]
            [r,s]      = map (fetchAttr as) ["reflexive","symmetric"]
        return DRel{inners=iv,size=sv,maxSize=mx,minSize=mn,reflexive=r,symmetric=s}

    -- Matrix
    fromEssence [xMatch| [i]  := domain.matrix.inner
                       | [ix] := domain.matrix.index |] = do
        iv  <- fromEssence i
        ixv <- fromEssence ix
        return DMat{inner=iv,innerIdx=ixv}

    fromEssence x = Left x


-- How to match domains
_matchingDoms :: Domain -> IO ()
_matchingDoms DSet{inner=DFunc{innerFrom=DInt{},innerTo=DInt{},total=True}}  =do
    putStrLn "Matches only set of function(total) int(1..2) --> int(1..2) "
    putStrLn "_matchingDoms $ dset{inner=dfunc{innerFrom=dintRange 1 2, innerTo=dintRange 1 2,total=True} }"
    putStrLn "_matchingDoms $ fromJust $ fromEssence [dMake| set of function(total) int(1..2) --> int(1..2)  |]"

_matchingDoms DMSet{inner=DFunc{innerFrom=DInt{},innerTo=DInt{}}}  =
    putStrLn "Matches mset of function int(1..2) --> int(1..2) with any attrs"

-- _matchingDoms (Set_ (Func_ (Set_ DInt{}) (DInt{}) ) ) = do
--     putStrLn "Using pattens"
--     putStrLn "Matches set of function set of int(1..2) --> int(1..2)"
--     putStrLn "_matchingDoms $ dset{inner=dfunc{innerFrom=dset{inner=dintRange 1 2}, innerTo=dintRange 1 2}}"
--
-- _matchingDoms (Set_ DFunc{innerFrom=DInt{},innerTo=DInt{},surjective=True} ) = do
--     putStrLn "Using pattens and records"
--     putStrLn "Matches set of function(surjective) set of int(1..2) --> int(1..2)"
--     putStrLn "_matchingDoms $ dset{inner=dfunc{innerFrom=dintRange 1 2, innerTo=dintRange 1 2,surjective=True}}"

-- -- A patten is a type level convenience, for patten matching
-- pattern Set_ a=
--     DSet{size = Nothing, minSize = Nothing, maxSize = Nothing, inner = a}
-- pattern Func_ a b=
--     DFunc{size = Nothing, minSize = Nothing, maxSize = Nothing
--          ,injective=False, surjective=False, total=False
--          ,innerFrom=a, innerTo=b}
-- pattern Set_Set_ a = Set_ (Set_ a)
