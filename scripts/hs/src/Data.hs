{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

{-# LANGUAGE DeriveGeneric #-}

module Data where
import Runner(RefineR,SolveR)
import EssenceDomain(EssenceDomain)
import EssenceConstraints(Eexpr)

import Language.E hiding (trace)
import GHC.Generics (Generic)
import Data.Aeson(FromJSON(..),ToJSON(..))
import Debug.Trace(trace)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B

--Since Monad has not been fixed yet
type MonadA   m  = (Monad m , Applicative m , Functor m )
type MonadGen m  = (Monad m , Applicative m , Functor m , MonadState GenState  m )
type MonadGG  m  = (Monad m , Applicative m , Functor m , MonadState GenGlobal m )
type Dom = E

data GenState = GenState
        {
          eFinds          :: [(Text, EssenceDomain)] -- Domains of finds
        , eConstraints    :: [Eexpr]     -- Constrints
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
        ,gInconsistent :: [SolveR]
        ,gMaxNesting   :: Int
        ,gCount        :: Int
    } deriving (Show,Generic)

defaultGenGlobal :: GenState
defaultGenGlobal = GenState{
        eFinds=[]
       ,eConstraints=[]
       ,eFindIndex=0
       ,eGen=undefined
       ,eMaxNesting=undefined
       }

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

    trace ("rangeRandomG:"
        ++ " range  " ++ show range
        ++ " result " ++ show x
        ++ " before " ++ show gen
        ++ " after  " ++ show gen')
        return $ fromIntegral x

