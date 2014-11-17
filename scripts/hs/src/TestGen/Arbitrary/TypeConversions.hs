{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, ScopedTypeVariables#-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE LambdaCase, MultiWayIf, TemplateHaskell, TupleSections #-}

module TestGen.Arbitrary.TypeConversions(toTypeWithConversions) where

import TestGen.Arbitrary.Helpers.Prelude
import TestGen.Arbitrary.Expr
import TestGen.Arbitrary.Literal
import TestGen.Arbitrary.Type

type PType = Type

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



reachableToType d oty@(TSet ity) = concatMapM process (types)
    where

    types =  catMaybes [ Just  $ TFunc ity TAny
                       , Just  $ TFunc TAny ity
                    --    , TRel TAny *| ity == TAny
                       , TRel <$> tupleInner ity
                       ]

    tupleInner :: Type -> Maybe [Type]
    tupleInner (TTuple ts) = Just ts
    tupleInner _           = Nothing

    process :: Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]


    process fty@(TRel itys) = funcs >>=
        concatMapM (uncurry (processCommon d) )

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
             simple fty [ (EProc . PtoSet, 1)]
                +| d - (fromInteger $ depthOf fty) >= 1


            ]


    process fty@(TFunc a b) = funcs >>=
        concatMapM (uncurry (processCommon d) )

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            -- 1 for func
            -- 1 usually for TFunc

            [ simple fty [ (EProc . Pdefined, 1) ]
                +| a == ity &&  d - (fromInteger $ depthOf a) > 2

            , simple fty [  (EProc . Prange, 1)  ]
                +| b == ity && d - (fromInteger $ depthOf b) > 2


            ,simple fty [ (EProc . PtoSet, 1)]
                +| d - (fromInteger $ max (depthOf a ) (depthOf b) ) > 2
                && typesUnify  (TTuple [a,b]) ity

            ,do
                nb <- purgeAny b
                (TFunc a nb,) <$> sequence [ preImage nb ]
                    +| a == ity
                    && d - (fromInteger $ max (depthOf a ) (depthOf nb) ) > 2
            ,do
                na <- purgeAny a
                (TFunc na b, ) <$> sequence  [image na]
                    +| b == ity
                    && d - (fromInteger $ max (depthOf na ) (depthOf b) ) > 2

            ]


        preImage :: PType -> GG (ToTypeFn, Depth)
        preImage pb = do
            ep <- withDepthDec (exprOf pb)
            return $ raise $ (EProc . (flip PpreImage) ep, 1)

        image :: PType -> GG (ToTypeFn, Depth)
        image pa = do
            ep <- withDepthDec (exprOf $ TSet pa)
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
    dotTypeFunc (o ,od) (i, oi) = (o. i, od + oi)



raise ::  (Expr -> Expr,Depth) -> (ToTypeFn, Depth)
raise (f,c) = ( \d -> d >>= return . f   , c)
-- raise (f,c) = (f, c)

simple
  :: (Functor f, Monad f)
  =>  t -> [(Expr -> Expr, Depth)]
  ->  f (t, [(ToTypeFn, Depth)])
simple ty val =  (ty,) <$>  mapM ( return . raise) val


infixl 1 *|

(*|) :: a -> Bool -> Maybe a
a  *| c | c = Just a
_  *| _    = Nothing

infixl 1 +|
(+|) :: Monad m =>  m a -> Bool -> m (Maybe a)
xs +| c | c = do
    x <- xs
    return (Just x)

_  +| _    = return Nothing
