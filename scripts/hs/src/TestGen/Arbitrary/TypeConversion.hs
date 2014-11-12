{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, ScopedTypeVariables#-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE LambdaCase, MultiWayIf, TemplateHaskell, TupleSections #-}

module TestGen.Arbitrary.TypeConversions(toTypeWithConversions) where

import TestGen.Arbitrary.Helpers.Prelude
import TestGen.Arbitrary.Expr
import TestGen.Arbitrary.Literal
import TestGen.Arbitrary.Type


toTypeWithConversions :: Type -> GG (Maybe Expr)
toTypeWithConversions ty = do
    d <- gets depth_
    addLog "toType" ["depth_" <+> pretty d, "ty" <+> pretty ty]

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

reachableToType _ TBool = return []

reachableToType d TAny = do
    newTy <- withDepth d atype
    reachableToType d newTy


reachableToType d oty@TInt = concatMapM process (allowed d)
    where

    allowed :: Depth -> [Type]
    allowed 0 = []
    allowed 1 = allowed 0 ++ [TBool]
    allowed _ = allowed 1 ++ [TSet TAny]

    process :: Type -> GG ( [ (Type, GG [ToTypeFn] )  ])
    process ty@TBool = do
        processCommon d ty [ EProc . PtoInt ]

    process ty@(TSet TAny) = do
        processCommon d ty [ EUniOp . UBar ]

    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]


reachableToType d oty@(TSet inner) = concatMapM process (allowed d)
    where
    tyDepth = depthOf inner

    allowed :: Depth -> [Type]
    allowed 0 = []
    allowed 1 = allowed 0 ++ []
    allowed 2 = allowed 1  ++ [] ++
        if | tyDepth == 0  -> [TFunc inner TAny ]
           | otherwise  -> []


    process :: Type -> GG ( [ (Type, GG [ToTypeFn] )  ])
    process ty@(TFunc inner _) =
        processCommon d ty [ EProc . Pdefined ]

    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]

reachableToType d oty@(TFunc from to) = return []


reachableToType d ty = ggError "reachableToType missing ty"
        ["ty"  <+> pretty ty, "depth" <+> pretty ty ]


processCommon :: Depth ->  Type -> [ToTypeFn] -> GG [(Type, GG [ToTypeFn] ) ]
processCommon d ty arr = do
    choices  <- reachableToType (d- 1) ty
    combined <- combine (ty, arr) choices
    return $  (ty, return arr) : combined

combine :: (Type, [ToTypeFn]) -> [(Type, GG [ToTypeFn])] -> GG [(Type, GG [ToTypeFn])]
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


sd :: GG ( Maybe Expr)
sd = return $ Just undefined

aa :: GG Expr
aa = do
    d <- map return <$> maybeToList <$> sd
    oneof2 $ d ++ [  boolLit ]
