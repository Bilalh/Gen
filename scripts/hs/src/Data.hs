{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Data where
-- import Language.E hiding(EssenceLiteral(..))
import Language.E

import Test.QuickCheck
import qualified Test.QuickCheck as Q
import Data.DeriveTH

data GenState = GenState
        {
          gFinds :: [(Text, E)] -- Domains of finds
        , gFindIndex :: Int     -- Next index for a name
        } deriving (Show)

class (Monad m,  MonadState GenState m) => MonadGen m where
    {}


-- data EssenceLiteral
--     = ELB Bool
--     | ELI Integer
--     | ELTuple [EssenceLiteral]
--     | ELSet [EssenceLiteral]
--     | ELMSet [EssenceLiteral]
--     | ELFunction [(EssenceLiteral, EssenceLiteral)]   -- list of mappings
--     | ELRelation [[EssenceLiteral]]                   -- list of tuples
--     | ELPartition [[EssenceLiteral]]                  -- list of parts
--     deriving (Eq, Ord, Show)

-- $( derive makeArbitrary ''EssenceLiteral )

class ArbitraryLimited a where
    pickVal :: Monad m => Int -> m a
    pickVal i = error "no default generator"

instance ArbitraryLimited EssenceLiteral where
    pickVal i = return $ ELI 3

ff :: Monad m => m EssenceLiteral
ff = pickVal 1
