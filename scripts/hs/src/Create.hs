{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
module Create where

import Data
import EssenceDomain
import EssenceConstraints
import ArbitraryDomains
import ToEssence
import Helpers

import Language.E

import qualified Data.Text as T

chooseSpec :: MonadGen m  => m Spec
chooseSpec = do
    varsNum <-  rangeRandomG (1,3)
    mapM_ (\_ -> chooseFindsDomain) [1..varsNum]
    chooseAllConstraints

    gs <- gets eFinds
    cons <- gets eConstraints
    constraints <- return $ [xMake| topLevel.suchThat := map toEssence cons |]

    let es = fmap (\(n,e) -> mkFind ((mkName n), toEssence e) ) gs
    return . mkSpec $ es ++ [constraints]

chooseFindsDomain :: MonadGen m => m ()
chooseFindsDomain = do
    levels <- gets eMaxNesting
    dom :: EssenceDomain  <- pickVal levels

    i <- gets eFindIndex
    let name = T.pack $  "var" ++  (show  i)
    fs <- gets eFinds
    modify ( \s-> s{eFindIndex = i+1
                   ,eFinds = (name, dom) : fs  }  )
    return ()

chooseAllConstraints :: MonadGen m => m ()
chooseAllConstraints = do
    finds <- gets eFinds
    constraints <- concatMapM chooseConstraints finds
    modify (\s -> s{eConstraints = Elit (ELB True) : constraints})

chooseConstraints :: MonadGen m => (Text,EssenceDomain) -> m [Eexpr]
chooseConstraints (name,(DInt _ u)) = do
    let expr = Eneq (Evar name)  ( Elit (ELI (u-1)) )
    return [expr]
