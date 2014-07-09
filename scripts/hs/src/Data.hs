{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE DeriveGeneric #-}

module Data where
import Helpers
import Runner

import Language.E
import GHC.Generics (Generic)
import Data.Aeson(FromJSON(..),ToJSON(..))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B

--Since Monad has not been fixed yet
type MonadA   m  = (Monad m , Applicative m , Functor m )
type MonadGen m  = (Monad m , Applicative m , Functor m , MonadState GenState  m )
type MonadGG  m  = (Monad m , Applicative m , Functor m , MonadState GenGlobal m )


data GenState = GenState
        {
          eFinds          :: [(Text, E)] -- Domains of finds
        , eFindIndex      :: Int         -- Next index for a name
        , eGen            :: StdGen      -- random seed
        , eMaxNesting     :: Int         -- Max level of nesting
        } deriving (Show)

data GenGlobal = GenGlobal
    {
         gSeed         :: Int
        ,gBase         :: FilePath
        ,gTotalTime    :: Float
        ,gSpecTime     :: Int
        ,gErrorsRefine :: [RefineR]
        ,gErrorsSolve  :: [SolveR]
        ,gMaxNesting   :: Int
        ,gCount        :: Int
    } deriving (Show,Generic)

instance FromJSON GenGlobal
instance ToJSON GenGlobal


saveAsJSON :: ToJSON a => a -> FilePath -> IO ()
saveAsJSON a fp = do
    let e = A.encode a
    B.writeFile fp e


rangeRandomG :: MonadGen m => (Int, Int) -> m Int
rangeRandomG range = do
    gen <- gets eGen
    let (x, gen') = randomR range gen
    modify $ \gl -> gl{ eGen = gen'}
    return $ fromIntegral x

data EssenceDomain =
      DInt  Integer Integer
    | DSet  EssenceDomain
    | DFunc EssenceDomain EssenceDomain
    | DPart EssenceDomain
    | DRel  [EssenceDomain]
    | DMat  EssenceDomain EssenceDomain -- index domain
    deriving(Show)


data EExpr =
       EGT EExpr EExpr
     | EVar Text
     | ELit EssenceLiteral


fromEssenceExpr :: EExpr -> E
fromEssenceExpr (EGT a b) = [eMake| &aa > &bb |]
    where
        aa = fromEssenceExpr a
        bb = fromEssenceExpr b

fromEssenceExpr (EVar (name) ) = mkName name
fromEssenceExpr (ELit lit )    = fromEssenceLiteral lit

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
    pickVal 1 = do
          l <- rangeRandomG (1,5)
          u <- rangeRandomG (l,5)
          return $ DInt (fromIntegral l) (fromIntegral u)
    pickVal l | l > 0 = do
        r <- rangeRandomG (2,6)
        case r of
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
                  index <- pickVal 1
                  dom   <- pickVal (l-1)
                  return $ DMat index dom
              _ -> error "pickVal EssenceDomain Impossible happen"

