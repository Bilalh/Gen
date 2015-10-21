{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Arbitrary.Common  where

import Gen.Arbitrary.Prelude

import qualified Data.Text as T


-- Returns a type that can be reached within the allowed depth
typeFromType ::Type -> GG Type
typeFromType ty = do
    depth_ <- gets depth_
    if
        | depth_ < 0  -> ggError "typeFromType depth 0" []
        | depth_ == 0 -> return ty
        | otherwise -> typeFromType' ty

    where

nextReachable :: GG Type  -> GG Type
nextReachable rs = do
    r ::Type <- rs
    withDepthDec (typeFromType r)

typeFromType' ::Type -> GG Type
typeFromType' ty@(TypeSet _) = oneof2 [
      return ty
    , reachable
    , nextReachable reachable
    ]

    where reachable = elements2 [ TypeInt ] -- with one step

typeFromType' ty = return ty
-- typeFromType' ty =  ggError "typeFromType' unmatched" [ pretty . show $ ty ]



nextQuanVarName :: GG Text
nextQuanVarName = do
    curNum <- gets nextNum_
    let varName = T.pack $ "q_" ++ show curNum
    modify (\s -> s{nextNum_=curNum + 1}  )
    return varName

introduceVariable :: Var -> GG ()
introduceVariable newVar =
    modify ( \st -> st{newVars_ = newVar : newVars_ st} )
