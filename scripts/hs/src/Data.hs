{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module Data where
import Helpers
import Runner

import Language.E
import GHC.Generics (Generic)
import Data.Aeson(FromJSON(..),ToJSON(..))

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Sequence as Seq



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
    | DSet  [SetAtrr] EssenceDomain
    | DFunc EssenceDomain EssenceDomain
    | DPart EssenceDomain
    | DRel  [EssenceDomain]
    | DMat  EssenceDomain EssenceDomain -- index domain
    deriving(Show)

data SetAtrr =
      SSize    Integer
    | SMaxSize Integer
    | SMinSize Integer
    deriving(Show)

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

    toEssence (DFunc a b) =  [dMake| function  &dom1 --> &dom2 |]
        where dom1 = toEssence a
              dom2 = toEssence b

    toEssence (DPart dom) =  [dMake| partition from &domE |]
        where domE = toEssence dom

    toEssence (DRel doms) =
        [xMake|domain.relation.inners                    := domsE
              |domain.relation.attributes.attrCollection := [] |]
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

    pickVal i = error . show $  "invaild nesting level " ++ show i



class ArbitraryAttr a where
    getAttrs :: MonadGen m =>  m [a]

instance ArbitraryAttr SetAtrr where
    getAttrs = do
        num <- rangeRandomG (0,3)
        if num == 0 then
            return []
        else do
            vs <- sample [SSize, SMinSize, SMaxSize] num
            mapM addInt vs

        where
        addInt :: MonadGen m => (Integer -> SetAtrr) -> m SetAtrr
        addInt f = do
            r <- rangeRandomG (1,5)
            return $ f (fromIntegral r)


-- because Module imports form a cycle:
sample :: MonadGen m => [a] -> Int -> m [a]
sample ys num | length ys == num = return ys
sample ys num =
   go 0 (l - 1) (Seq.fromList ys)
   where
   l =  length ys
   go !n !i xs | n > num =  return $!  (toList . Seq.drop (l - num)) xs
               | otherwise = do
                   j <- rangeRandomG (0, i)
                   let toI = xs `Seq.index` j
                       toJ = xs `Seq.index` i
                       next = (Seq.update i toI . Seq.update j toJ) xs
                   go (n + 1)  (i - 1) next
