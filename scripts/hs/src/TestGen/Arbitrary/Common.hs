{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}

module TestGen.Arbitrary.Common where

import Language.E
import AST.Imports
import TestGen.Arbitrary.Helpers
import TestGen.Arbitrary.Data
import TestGen.Arbitrary.Type

import Test.QuickCheck

import Text.Groom(groom)

import qualified Data.Text as T
import qualified Data.Map as M




-- Returns a type that can be reached within the allowed depth
typeFromType :: SpecState -> Type -> Gen Type
typeFromType SS{..} ty | depth_ < 1  = elements [ty]
typeFromType s@SS{..} ty@(TSet _) = oneof [
      return ty
    , reachable
    , do
        r <- reachable
        typeFromType s{depth_=depth_ - 1} r
    ]

    where reachable = elements [ TInt ]

typeFromType s@SS{..} ty@(TMatix _) = oneof [
        return ty
    ]

typeFromType s@SS{..} ty@TInt = oneof [
        return ty
    ]

typeFromType s@SS{..} ty@TBool = oneof [
        return ty
    ]

typeFromType s ty = docError [
    "typeFromType unmatched",
    pretty . show $ ty,
    pretty s
    ]


lookupType :: SpecState -> Ref -> Type
lookupType  s@SS{..} name =
    case  name `lookup` newVars_ of
        Just v  -> v
        Nothing ->
            case fmap (typeOfDom . domOfFG) $  name `M.lookup` doms_ of
                Nothing ->  docError ["lookUpType", pretty s, pretty name]
                Just v  -> v

nextQuanVarName :: SpecState -> (SpecState, Text)
nextQuanVarName s@SS{..} =
    let varName = T.pack $ "q_" ++ show nextNum_
        s' = s{nextNum_=nextNum_ + 1}
    in  (s', varName)

introduceVariable :: SpecState -> (Text,Type) -> SpecState
introduceVariable s@SS{..} var =
    s{newVars_= newVar : newVars_ }
    where
    newVar = var
