{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, ScopedTypeVariables#-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE LambdaCase, MultiWayIf, TemplateHaskell, TupleSections #-}

module TestGen.Arbitrary.TypeConversions(toTypeWithConversions) where

import TestGen.Arbitrary.Helpers.Prelude
import TestGen.Arbitrary.Expr
import TestGen.Arbitrary.Literal
import TestGen.Arbitrary.Type

-- for ghci usage
_aa ::Type ->  GG (Maybe Expr)
_aa ty = do
    toTypeWithConversions ty >>= \case
        Nothing -> return Nothing
        Just xs -> do
            xx <- xs
            return $ Just xx



toTypeWithConversions :: Type -> GG (Maybe (GG Expr))
toTypeWithConversions ty = do
    d <- gets depth_
    addLog "toType" ["depth_" <+> pretty d, "ty" <+> pretty ty]

    -- Simple cases
    if
        | d < 0  -> ggError "toTypeWithConversions depth_ <0" ["ty:" <+> pretty ty]
        | d == 0 ->  return Nothing
        | otherwise  -> con ty


con :: Type -> GG (Maybe (GG Expr))
con  to  = do
    d <- gets depth_

    reachableToType d to >>= \case
        [] -> return Nothing
        choices -> do
            (fromTy, toFn) <- elements2 choices
            addLog "toType:choices"
                [ "fromTy" <+> pretty fromTy
                , "d"      <+> pretty d
                , "to"     <+> pretty to
                ]
            fs <- toFn
            (f, depthNeeded) <- elements2 fs
            fromExpr <- withDepth (d - depthNeeded) (exprOfPurgeAny fromTy)
            return $  Just $ f (return fromExpr)



type ToTypeFn = (GG Expr -> GG Expr)

---- Give an type return the possible ways to get to that type
---- e.g  TInt  ->  Just $  [TSet Tint,  [  | x |  ]   ]

-- return (the type, (function applied and depth needed)  )

reachableToType :: Depth -> Type -> GG [ (Type, GG [(ToTypeFn, Depth)] ) ]
reachableToType 0 _ = return  []

reachableToType _ TBool = return []

reachableToType d TAny = do
    newTy <- withSameDepth atype
    reachableToType d newTy

reachableToType d oty@TInt = concatMapM process (allowed d)
    where

    allowed :: Depth -> [Type]
    allowed 0 = []
    allowed 1 = allowed 0 ++ [TBool]
    allowed _ = allowed 1 ++ [TSet TAny]

    process :: Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]
    process ty@TBool = do
        processCommon d ty [ raise ( EProc . PtoInt, 1) ]

    process ty@(TSet TAny) = do
        processCommon d ty [ raise (EUniOp . UBar, 1) ]

    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]



reachableToType d oty@(TSet ity) = concatMapM process (allowed d)
    where
    tyDepth = depthOf ity

    allowed :: Depth -> [Type]
    allowed 0 = []
    allowed 1 = allowed 0 ++ []
    allowed _ = allowed 1  ++ [] ++
        if | tyDepth == 0  -> [TFunc ity TAny,  TFunc TAny ity]
           | otherwise  -> []

    process :: Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]
    process ty@(TFunc a b ) = do
        fs :: [[(ToTypeFn, Depth)]] <- funcs
        processCommon d ty (concat fs)

        where
        da = depthOf a
        db = depthOf b


        funcs :: GG [[(ToTypeFn, Depth)]]
        funcs = sequence
            [ mapM ( return . raise) [  (EProc . Pdefined, 1)  ]
                *| a == ity &&  d - (fromInteger da) - 1 > 0

            , mapM ( return . raise) [  (EProc . Prange, 1)  ]
                *| b == ity && d - (fromInteger db) - 1 > 0

            , sequence [ preImage ]
                *| d >2 && a == ity

            , sequence [ image ]
                *| d >3 && b== ity

            ]


        preImage :: GG (ToTypeFn, Depth)
        preImage = do
            ep <- withDepthDec (exprOfPurgeAny b)
            return $ raise $ (EProc . (flip PpreImage) ep, 1)

        image :: GG (ToTypeFn, Depth)
        image = do
            ep <- withDepthDec (exprOfPurgeAny $ TSet a)
            return $ raise $ (EProc . Pimage ep, 1)

    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]


reachableToType d oty@(TTuple inner)  = return []
reachableToType d oty@(TMatix inner)  = return []
reachableToType d oty@(TMSet inner)   = return []
reachableToType d oty@(TFunc from to) = return []
reachableToType d oty@(TRel inners)   = return []
reachableToType d oty@(TPar inners)   = return []
reachableToType d oty@(TUnamed _)     = return []
reachableToType d oty@(TEnum _)       = return []


processCommon :: Depth ->  Type -> [(ToTypeFn,Depth)] -> GG [(Type, GG [(ToTypeFn,Depth)] ) ]
processCommon _ _ [] =  return []
processCommon d ty arr = do
    choices  <- reachableToType (d- 1) ty
    combined <- combine (ty, arr) choices
    return $  (ty, return arr) : combined

combine :: (Type, [(ToTypeFn,Depth)])
        -> [(Type, GG [(ToTypeFn,Depth)])]
        -> GG [(Type, GG [(ToTypeFn,Depth)])]
combine _ []       = return []
combine (_, ff) xs = do
    res <- mapM (mapper ff) xs

    return $  res

    where
    mapper :: [(ToTypeFn,Depth)]
           -> (Type, GG [(ToTypeFn,Depth)])
           -> GG (Type, GG [(ToTypeFn,Depth)] )
    mapper outer (innerTy, innerW)  = do
        inner <-  innerW

        let xx  = [ dotTypeFunc o  i |  i <- inner, o <- outer   ]

        return $ (innerTy, return xx)

    dotTypeFunc :: (ToTypeFn,Depth) -> (ToTypeFn,Depth) -> (ToTypeFn,Depth)
    dotTypeFunc (o,od) (i, oi) = (o. i, od + oi)

raise ::  (Expr -> Expr,Depth) -> (ToTypeFn, Depth)
raise (f,c) = ( \d -> d >>= return . f   , c)
-- raise (f,c) = (f, c)


raiseExpr :: (a -> b) -> GG a -> GG b
raiseExpr f = (=<<) (return . f)

infixl 1 *|

(*|) :: Monad m =>  m [a] -> Bool -> m [a]
xs *| c | c = xs
_  *| _    = return []
