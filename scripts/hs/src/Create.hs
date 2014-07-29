{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
module Create where

import Data
import Essence
import Helpers

import Language.E

import qualified Data.Text as T

chooseSpec :: MonadGen m  => m Spec
chooseSpec = do
    varsNum <-  rangeRandomG (1,3)
    mapM_ (\_ -> chooseFindsDomain) [1..varsNum]

    gs <- gets eFinds
    let es = fmap (\(n,e) -> mkFind ((mkName n), e) ) gs
    return . mkSpec $ es



chooseFindsDomain :: MonadGen m => m ()
chooseFindsDomain = do
    levels <- gets eMaxNesting
    dom :: EssenceDomain  <- pickVal levels

    i <- gets eFindIndex
    let name = T.pack $  "var" ++  (show  i)
    fs <- gets eFinds
    modify ( \s-> s{eFindIndex = i+1
                   ,eFinds = (name,  toEssence dom) : fs  }  )
    return ()


