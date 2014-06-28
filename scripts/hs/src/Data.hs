{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}


module Data where
import Helpers

import Language.E


-- import Test.QuickCheck
-- import qualified Test.QuickCheck as Q
-- import Data.DeriveTH

--Since Monad has not been fixed yet
type MonadA m = (Monad m, Applicative m, Functor m)
type MonadGen m = (Monad m, Applicative m, Functor m, MonadState GenState m)

data GenState = GenState
        {
          gFinds :: [(Text, E)] -- Domains of finds
        , gFindIndex :: Int     -- Next index for a name
        , genSeed :: StdGen
        } deriving (Show)

rangeRandomG :: MonadGen m => (Int, Int) -> m Int
rangeRandomG range = do
    gen <- gets genSeed
    let (x, gen') = randomR range gen
    modify $ \gl -> gl{ genSeed = gen'}
    return $ fromIntegral x

data EssenceDomain =
      DInt  Integer Integer
    | DSet  EssenceDomain
    | DFunc EssenceDomain EssenceDomain
    | DPart EssenceDomain
    | DRel  [EssenceDomain]
    | DMat  EssenceDomain EssenceDomain -- index domain
    deriving(Show)


fromEssenceDomain :: EssenceDomain -> E
fromEssenceDomain (DInt lower upper) = [dMake| int(&l..&u) |]
    where l = mkInt lower
          u = mkInt upper

fromEssenceDomain (DSet dom) =  [dMake| set of &domE |]
    where domE = fromEssenceDomain dom

fromEssenceDomain (DFunc a b) =  [dMake| function  &dom1 --> &dom2 |]
    where dom1 = fromEssenceDomain a
          dom2 = fromEssenceDomain b

fromEssenceDomain (DPart dom) =  [dMake| partition from &domE |]
    where domE = fromEssenceDomain dom

fromEssenceDomain (DRel doms) =
    [xMake|domain.relation.inners                    := domsE
          |domain.relation.attributes.attrCollection := [] |]
    where domsE = map fromEssenceDomain doms

fromEssenceDomain (DMat index dom) = [dMake| matrix indexed by [&indexE] of &domE |]
    where indexE = fromEssenceDomain index
          domE   = fromEssenceDomain dom

class ArbitraryLimited a where
    pickVal :: MonadGen m  => Int ->  m a
    pickVal _ = error "no default generator"

instance ArbitraryLimited EssenceDomain where
    -- 0 always gives back a int for things like matrix indexing
    pickVal 0 = do
          l <- rangeRandomG (1,10)
          u <- rangeRandomG (l,10)
          return $ DInt (fromIntegral l) (fromIntegral u)
    pickVal l =do
        r <- rangeRandomG (1,6)
        case r of
              1 -> pickVal 0
              2 -> pure DSet <*> pickVal (l-1)
              3 -> do
                  a <- pickVal (l-1)
                  b <- pickVal (l-1)
                  return $ DFunc a b
                  -- Does not update randomgen when using multiple <*>
                  -- pure DFunc <*> pickVal (l-1) <*> pickVal (l-1)
              4 -> pure DPart <*> pickVal (l-1)
              5 -> do
                 num <- rangeRandomG (1,3)
                 doms <- mapM (\_ -> pickVal (l-1) ) [1..num]
                 return $ DRel doms
              6 -> do
                  index <- pickVal 0
                  dom   <- pickVal (l-1)
                  return $ DMat index dom
              _ -> error "pickVal EssenceDomain Impossible happen"


