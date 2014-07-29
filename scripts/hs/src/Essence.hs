{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Essence where

import Data
import Sample(sample,weightedRange)
import Helpers

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

data EExpr =
       EGT EExpr EExpr
     | EVar Text
     | ELit EssenceLiteral


class ToEssence a where
    toEssence :: a -> E

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

instance ToEssence EssenceLiteral where
    toEssence lit = fromEssenceLiteral lit

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

instance ToEssence EExpr where
    toEssence (EGT a b) = [eMake| &aa > &bb |]
        where
            aa = toEssence a
            bb = toEssence b

    toEssence (EVar (name) ) = mkName name
    toEssence (ELit lit )    = toEssence lit


class ArbitraryLimited a where
    pickVal :: MonadGen m  => Int ->  m a
    pickVal _ = error "no default generator"

instance ArbitraryLimited EssenceDomain where
    -- 1 always gives back a int for things like matrix indexing
    pickVal 1 = do
          l <- rangeRandomG (1,5)
          u <- rangeRandomG (l,5)
          return $ DInt (fromIntegral l) (fromIntegral u)
    pickVal l | l > 0 = do
        r <- rangeRandomG (2,6)
        case r of
              2 -> do
                innerDom <- pickVal (l-1)
                attrs <- getAttrs
                return $ DSet attrs innerDom
              3 -> do
                a <- pickVal (l-1)
                b <- pickVal (l-1)
                attrs <- getAttrs
                return $ DFunc attrs a b
                -- Does not update randomgen when using multiple <*>
                -- pure DFunc <*> pickVal (l-1) <*> pickVal (l-1)

              4 -> do
                attrs <- getAttrs
                pickVal (l-1) >>= return . DPart attrs
              5 -> do
                num <- rangeRandomG (1,3)
                doms <- mapM (\_ -> pickVal (l-1) ) [1..num]
                attrs <- getAttrs
                return $ DRel attrs doms
              6 -> do
                index <- pickVal 1
                dom   <- pickVal (l-1)
                return $ DMat index dom
              _ -> error "pickVal EssenceDomain Impossible happen"

    pickVal i = error . show $  "invaild nesting level " ++ show i


class ArbitraryAttr a where
    getAttrs :: MonadGen m =>  m [a]

instance ArbitraryAttr SetAtrr where
    getAttrs =
        getWeightedAttrs [SSize, SMinSize, SMaxSize]

instance ArbitraryAttr FuncAttr where
    getAttrs =
        getWeightedAttrs [FSize,FMinSize, FMaxSize,
                          k FInjective, k FSurjective, k FTotal ]
        where
        k  f _ =  f

instance ArbitraryAttr PartAttr where
    getAttrs =
        getWeightedAttrs [PNumParts, PMaxNumParts, PMinNumParts, PPartSize,
                          PMaxPartSize, PMinPartSize, k PRegular, k PComplete,
                          PSize, PMaxSize, PMinSize]
        where
        k  f _ =  f

instance ArbitraryAttr RelAttr where
    getAttrs = getWeightedAttrs [RSize, RMaxSize, RMinSize]

getWeightedAttrs :: MonadGen m => [Integer -> a] -> m [a]
getWeightedAttrs attrs = do
        num <- weightedRange (length attrs)
        if num == 0 then
            return []
        else do
            vs <- sample attrs num
            mapM withInt vs

withInt :: MonadGen m => (Integer -> a) -> m a
withInt f = do
    r <- rangeRandomG (1,5)
    return $ f (fromIntegral r)



