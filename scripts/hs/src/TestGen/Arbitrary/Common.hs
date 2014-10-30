{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}

module TestGen.Arbitrary.Common  where

import TestGen.Arbitrary.Helpers.Prelude
import TestGen.Arbitrary.Type

import qualified Data.Map as M
import qualified Data.Text as T


-- Returns a type that can be reached within the allowed depth
typeFromType :: Type -> GG Type
typeFromType ty = do
    depth_ <- gets depth_
    if
        | depth_ < 0  -> ggError "typeFromType depth 0" []
        | depth_ == 0 -> return ty
        | otherwise -> typeFromType' ty

    where

nextReachable :: GG Type  -> GG Type
nextReachable rs = do
    r :: Type <- rs
    withDepthDec (typeFromType r)

typeFromType' :: Type -> GG Type
typeFromType' ty@(TSet _) = oneof2 [
      return ty
    , reachable
    , nextReachable reachable
    ]

    where reachable = elements2 [ TInt ]

typeFromType' ty = return ty
typeFromType' ty =  ggError "typeFromType' unmatched" [ pretty . show $ ty ]


lookupType ::  Ref -> GG Type
lookupType  name = do
    s@SS{newVars_,doms_} <- get

    case name `lookup` newVars_ of
        Just v  -> return v
        Nothing ->
            case fmap (typeOfDom . domOfFG) $  name `M.lookup` doms_ of
                Nothing ->  ggError "lookUpType"  [pretty name]
                Just v  -> return v


nextQuanVarName :: GG Text
nextQuanVarName = do
    curNum <- gets nextNum_
    let varName = T.pack $ "q_" ++ show curNum
    modify (\s -> s{nextNum_=curNum + 1}  )
    return varName

introduceVariable :: (Text,Type) -> GG ()
introduceVariable newVar =
    modify ( \st -> st{newVars_ = newVar : newVars_ st} )
