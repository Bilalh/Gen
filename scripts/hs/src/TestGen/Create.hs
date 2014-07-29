{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
module TestGen.Create where

import TestGen.ArbitraryDomains
import TestGen.Data
import TestGen.EssenceConstraints
import TestGen.EssenceDomain
import TestGen.Helpers
import TestGen.ToEssence

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

chooseConstraints _ = return []

findsOfType :: MonadGen m => EssenceDomain ->  m [(Text, EssenceDomain)]
findsOfType edom = do
    finds <- gets eFinds
    let ofType =   [  x | x <- finds, isSameType edom (snd x)]
    return $ finds

isSameType :: EssenceDomain -> EssenceDomain -> Bool
isSameType (DInt _ _) (DInt _ _) = True
