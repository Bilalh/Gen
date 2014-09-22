{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module TestGen.EssenceDomain where

import AST.ToEssence
import Common.Helpers

import Language.E

data EssenceDomain =
      DInt  Integer Integer
    | DSet  [SetAtrr] EssenceDomain
    | DFunc [FuncAttr] EssenceDomain EssenceDomain
    | DPart [PartAttr] EssenceDomain
    | DRel  [RelAttr]  [EssenceDomain]
    | DMat  EssenceDomain EssenceDomain -- index domain
    deriving(Show)

data SetAtrr =
      SSize    Integer
    | SMaxSize Integer
    | SMinSize Integer
    deriving(Show)

data FuncAttr =
      FSize    Integer
    | FMaxSize Integer
    | FMinSize Integer
    | FInjective
    | FSurjective
    | FTotal
    deriving(Show)

data PartAttr =
      PSize        Integer
    | PMaxSize     Integer
    | PMinSize     Integer
    | PNumParts    Integer
    | PMaxNumParts Integer
    | PMinNumParts Integer
    | PPartSize    Integer
    | PMaxPartSize Integer
    | PMinPartSize Integer
    | PRegular
    | PComplete
    deriving (Show)

data RelAttr =
      RSize        Integer
    | RMaxSize     Integer
    | RMinSize     Integer
    deriving (Show)


instance ToEssence SetAtrr where
    toEssence (SSize    i) = mkAttr ("size"    , Just i)
    toEssence (SMaxSize i) = mkAttr ("maxSize" , Just i)
    toEssence (SMinSize i) = mkAttr ("minSize" , Just i)

instance ToEssence FuncAttr where
    toEssence (FSize    i)  = mkAttr ("size"       , Just i)
    toEssence (FMaxSize i)  = mkAttr ("maxSize"    , Just i)
    toEssence (FMinSize i)  = mkAttr ("minSize"    , Just i)
    toEssence (FTotal)      = mkAttr ("total"      , Nothing)
    toEssence (FInjective)  = mkAttr ("injective"  , Nothing)
    toEssence (FSurjective) = mkAttr ("surjective" , Nothing)

instance ToEssence PartAttr where
    toEssence (PSize    i     ) = mkAttr ("size"        , Just i  )
    toEssence (PMaxSize i     ) = mkAttr ("maxSize"     , Just i  )
    toEssence (PMinSize i     ) = mkAttr ("minSize"     , Just i  )
    toEssence (PNumParts    i ) = mkAttr ("numParts"    , Just i  )
    toEssence (PMaxNumParts i ) = mkAttr ("maxNumParts" , Just i  )
    toEssence (PMinNumParts i ) = mkAttr ("minNumParts" , Just i  )
    toEssence (PPartSize    i ) = mkAttr ("partSize"    , Just i  )
    toEssence (PMaxPartSize i ) = mkAttr ("maxPartSize" , Just i  )
    toEssence (PMinPartSize i ) = mkAttr ("minPartSize" , Just i  )
    toEssence (PComplete      ) = mkAttr ("complete"    , Nothing )
    toEssence (PRegular       ) = mkAttr ("regular"     , Nothing )

instance ToEssence RelAttr where
    toEssence (RSize    i     ) = mkAttr ("size"        , Just i  )
    toEssence (RMaxSize i     ) = mkAttr ("maxSize"     , Just i  )
    toEssence (RMinSize i     ) = mkAttr ("minSize"     , Just i  )


instance ToEssence EssenceDomain where
    toEssence (DInt lower upper) = [dMake| int(&l..&u) |]
        where l = mkInt lower
              u = mkInt upper

    toEssence (DSet attrs dom) =
        let e = [dMake| set of &domE |] in
        addAttrs (map toEssence attrs ) e
        where domE = toEssence dom

    toEssence (DFunc attrs a b) =
        let e = [dMake| function  &dom1 --> &dom2 |] in
        addAttrs (map toEssence attrs ) e
        where dom1 = toEssence a
              dom2 = toEssence b

    toEssence (DPart attrs dom) =
        let e = [dMake| partition from &domE |] in
        addAttrs (map toEssence attrs ) e
        where domE = toEssence dom

    toEssence (DRel attrs doms) =
        [xMake|domain.relation.inners                    := domsE
              |domain.relation.attributes.attrCollection := (map toEssence attrs ) |]
        where domsE = map toEssence doms

    toEssence (DMat index dom) = [dMake| matrix indexed by [&indexE] of &domE |]
        where indexE = toEssence index
              domE   = toEssence dom
