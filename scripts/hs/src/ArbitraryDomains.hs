{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module ArbitraryDomains where

import Data
import EssenceDomain
import Sample(sample,weightedRange)


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



