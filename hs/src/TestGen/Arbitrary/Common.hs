{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Common  where

import TestGen.Prelude
import TestGen.Arbitrary.Type

import qualified Data.Map as M
import qualified Data.Text as T


-- Returns a type that can be reached within the allowed depth
typeFromType :: TType -> GG TType
typeFromType ty = do
    depth_ <- gets depth_
    if
        | depth_ < 0  -> ggError "typeFromType depth 0" []
        | depth_ == 0 -> return ty
        | otherwise -> typeFromType' ty

    where

nextReachable :: GG TType  -> GG TType
nextReachable rs = do
    r :: TType <- rs
    withDepthDec (typeFromType r)

typeFromType' :: TType -> GG TType
typeFromType' ty@(TSet _) = oneof2 [
      return ty
    , reachable
    , nextReachable reachable
    ]

    where reachable = elements2 [ TInt ] -- with one step

typeFromType' ty = return ty
-- typeFromType' ty =  ggError "typeFromType' unmatched" [ pretty . show $ ty ]


lookupType ::  Ref -> GG TType
lookupType  name = do
    SS{newVars_,doms_} <- get

    case name `lookup` newVars_ of
        Just v  -> return v
        Nothing ->
            case fmap (typeOfDom . domOfGF) $  name `M.lookup` doms_ of
                Nothing ->  ggError "lookUpType"  [pretty name]
                Just v  -> return v


nextQuanVarName :: GG Text
nextQuanVarName = do
    curNum <- gets nextNum_
    let varName = T.pack $ "q_" ++ show curNum
    modify (\s -> s{nextNum_=curNum + 1}  )
    return varName

introduceVariable :: (Text, TType) -> GG ()
introduceVariable newVar =
    modify ( \st -> st{newVars_ = newVar : newVars_ st} )
