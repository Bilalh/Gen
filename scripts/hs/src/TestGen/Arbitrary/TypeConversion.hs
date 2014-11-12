{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, ScopedTypeVariables#-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE LambdaCase, MultiWayIf, TemplateHaskell, TupleSections #-}

module TestGen.Arbitrary.TypeConversions(toTypeWithConversions) where

import TestGen.Arbitrary.Helpers.Prelude
import TestGen.Arbitrary.Expr
import TestGen.Arbitrary.Literal



toTypeWithConversions :: Type -> GG (Maybe Expr)
toTypeWithConversions ty = do
    d <- gets depth_
    addLog "toType" ["depth_" <+> pretty d]

    -- Simple cases
    if
        | d < 0  -> ggError "toTypeWithConversions depth_ <0" ["ty:" <+> pretty ty]
        | d == 0 ->  return Nothing
        | otherwise  -> con d ty


con :: Depth -> Type -> GG (Maybe (Expr))
con _ to  = do
    d <- gets depth_

    reachableToType d to >>= \case
        [] -> return Nothing
        choices -> do
            (fromTy, toFn) <- elements2 choices
            fromExpr <- withDepthDec (exprOf fromTy)
            fs <- toFn
            f <- elements2 fs
            return $  Just $ f fromExpr



type ToTypeFn = (Expr -> Expr)

-- Give an type return the possible ways to get to that type
-- e.g  TInt  ->  Just $  [TSet Tint,  [  | x |  ]   ]

reachableToType :: Depth -> Type -> GG [ (Type, GG [ToTypeFn] )  ]
reachableToType 0 _ = return  []
reachableToType _ ( TSet (TTuple [a, b] )) = return $  [ (TFunc a b,
             return $  [ EProc . PtoSet ]
        )]

reachableToType _ TBool = return []


reachableToType d TInt =  do

    res <- concatMapM inner (allowed d)
    return res

    where

    allowed :: Depth -> [Type]
    allowed 0 = []
    allowed 1 = [TBool]
    allowed 2 = allowed 1 ++ []
    allowed _ = allowed 2 ++ [TSet (TTuple [TInt, TInt] )]


    inner :: Type -> GG ( [ (Type, GG [ToTypeFn] )  ])
    inner ty@TBool = do
        innerCommon ty [ EProc . PtoInt ]

    inner ty@(TSet (TTuple [TInt, TInt] )) = do
        innerCommon ty [ EUniOp . UBar ]


    innerCommon :: Type -> [Expr -> Expr] -> GG ( [ (Type, GG [ToTypeFn] )  ])
    innerCommon ty arr = do
        choices  <- reachableToType (d- 1) ty
        combined <- combine (ty, arr) choices
        return $  (ty, return arr) : combined


combine ::  (Type, [ToTypeFn]) ->  [(Type, GG [ToTypeFn])] -> GG ( [(Type, GG [ToTypeFn])])
combine _ []       = return []
combine (_, ff) xs = do
    res <- mapM (mapper ff) xs

    return $  res

    where
    mapper :: [ToTypeFn] -> (Type, GG [ToTypeFn]) -> GG (Type, GG [ToTypeFn] )
    mapper outer (innerTy, innerW)  = do
        inner <-  innerW

        let xx  = [ o . i |  i <- inner, o <- outer   ]

        return $ (innerTy, return xx)


sd :: GG ( Maybe ( Expr))
sd = return $ Just undefined

aa :: GG Expr
aa = do
    d <- map return <$> maybeToList <$> sd
    oneof2 $ d ++ [  boolLit ]
