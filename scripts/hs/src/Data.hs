{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}


{-# LANGUAGE ScopedTypeVariables #-}

module Data where
-- import Language.E hiding(EssenceLiteral(..))
import Language.E
import Control.Monad.Trans.State.Strict(StateT)
import Helpers

-- import Test.QuickCheck
-- import qualified Test.QuickCheck as Q
-- import Data.DeriveTH

-- Since Monad has not been fixed yet
class  (Monad m, Applicative m, Functor m) => MonadA m where
    {}



data GenState = GenState
        {
          gFinds :: [(Text, E)] -- Domains of finds
        , gFindIndex :: Int     -- Next index for a name
        , genSeed :: StdGen
        } deriving (Show)



-- class (Monad m,  MonadState GenState m, RandomM m) => MonadGen m where
class (MonadState GenState m) => MonadGen m where
    {}

rangeRandomG :: (Monad m) => (Int, Int) -> StateT GenState m Int
rangeRandomG range = do
    gen <- gets genSeed
    let (x, gen') = randomR range gen
    modify $ \gl -> gl{ genSeed = gen'}
    return $ fromIntegral x

data EssenceDomain =
      DInt Integer Integer
    | DSet EssenceDomain
    deriving(Show)


fromEssenceDomain :: EssenceDomain -> E
fromEssenceDomain (DInt lower upper) = [dMake| int(&l..&u) |]
    where l = mkInt lower
          u = mkInt upper

fromEssenceDomain (DSet dom) =  [dMake| set of &domE |]
    where domE = fromEssenceDomain dom

class ArbitraryLimited a where
    pickVal :: (Monad m, Applicative m, Functor m) => Int -> StateT GenState m a
    pickVal i = error "no default generator"


instance ArbitraryLimited EssenceDomain where
    pickVal 0 = do
          l <- rangeRandomG (1,10)
          u <- rangeRandomG (l,10)
          return $ DInt (fromIntegral l) (fromIntegral u)
    pickVal l =do
        r <- rangeRandomG (1,2)
        case r of
              1 -> pickVal 0
              2 -> pure DSet <*> pickVal (l-1)

