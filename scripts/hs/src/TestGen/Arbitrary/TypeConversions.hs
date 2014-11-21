{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, ScopedTypeVariables#-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE LambdaCase, MultiWayIf, TemplateHaskell, TupleSections #-}

module TestGen.Arbitrary.TypeConversions(toTypeWithConversions) where

import TestGen.Prelude
import TestGen.Arbitrary.Expr
import TestGen.Arbitrary.Literal
import TestGen.Arbitrary.Type

-- Type that does not have any anys
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

    reachableToTypeWithCommon d to >>= \case
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
            addLog "toType:choices" 
                [ nn "depthNeeded" depthNeeded  ]
            
            fromExpr <- withDepth (d - depthNeeded) (exprOfPurgeAny fromTy)
            return $  Just $ f (return fromExpr)



type ToTypeFn = (GG Expr -> GG Expr)

---- Give an type return the possible ways to get to that type
---- e.g  TInt  ->  Just $  [TSet Tint,  [  | x |, 1  ]   ]

-- return (the type, (function applied and depth needed)  )

reachableToTypeWithCommon :: Depth -> Type -> GG [ (Type, GG [(ToTypeFn, Depth)] ) ]
reachableToTypeWithCommon 0 _ =  return []
reachableToTypeWithCommon d y =  do
    rs <- reachableToType d y
    cs  <- common
    return (cs ++ rs)


    where
    common :: GG [(Type, GG [(ToTypeFn, Depth)])]
    common | isOrdered y && d > 2  =
        funcs >>= concatMapM (uncurry (processCommon d))

           | otherwise = return []
               
    funcs = do 
        addLog "reTyCommon" [ nn "y" y, nn "d" d  ]
        catMaybes <$> sequence 
            [
                do
                nty <- TSet <$> purgeAny y
                (nty,) <$>  sequence [ min, max ]
                    +| d - (fromInteger $ depthOf nty) >= 2
                    
            ,   do
                nty <- TMSet <$> purgeAny y
                (nty,) <$> sequence [ min, max ]
                    +| d - (fromInteger $ depthOf nty) >= 2
            ] 

    min, max :: GG (ToTypeFn, Depth)
    min = return $ raise $ (EProc . Pmin, 1)
    max = return $ raise $ (EProc . Pmax, 1)
    


reachableToType :: Depth -> Type -> GG [ (Type, GG [(ToTypeFn, Depth)] ) ]
reachableToType 0 _ = return  []


reachableToType d TAny = do
    newTy <- withSameDepth atype
    reachableToType d newTy

reachableToType d oty@TBool = concatMapM process types
    where

    types =  catMaybes
        [
            TPar TAny       *| d >= 2
        ,   TAny            *| d >= 2
        ,   TFunc TAny TAny *| d >= 2
        ]

    process :: Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]
    process fty@(TAny) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                do
                innerTy <- withDepth (d - 2) atype
                container <- elements2 [TSet, TMSet ]
                --TODO  could also allow the other way around
                (innerTy,) <$> sequence [ element $ container innerTy ]
                    +| True

            ]

        element :: PType -> GG (ToTypeFn, Depth)
        element i = do
            e1 <- withDepthDec $ exprOf i
            return $ raise $ (EBinOp . (flip BIn) e1, 1)

    process fty@(TFunc _ _ ) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
            do
            (TFunc a b) <- withDepthDec (purgeAny fty)

            op <-  elements2 [ Pinverse , flip Pinverse  ]
            (t1,t2) <- elements2 [ (TFunc a b, TFunc b a ),  (TFunc b a, TFunc a b ) ]
            e1 <- withDepthDec (exprOf t1)

            (t2,) <$> sequence [ return . raise $ (EProc . op e1, 1) ]
                +| d - (max (fromInteger $ depthOf t1 ) (fromInteger $ depthOf t2 )) > 1
            ]


    process fty@(TPar _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                do
                nty@(TPar inn) <- purgeAny fty
                (nty,) <$>  sequence [ together inn, apart inn  ]
                    +| d - (fromInteger $ depthOf nty) >= 1

            ]

        together :: PType -> GG (ToTypeFn, Depth)
        together i = do
            e1 <- withDepthDec $ exprOf i
            e2 <- withDepthDec $ exprOf i
            return $ raise $ (EProc . Ptogether e1 e2, 1)

        apart :: PType -> GG (ToTypeFn, Depth)
        apart i = do
            e1 <- withDepthDec $ exprOf i
            e2 <- withDepthDec $ exprOf i
            return $ raise $ (EProc . Papart e1 e2, 1)

    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]


reachableToType d oty@TInt = concatMapM process types
    where

    types =  catMaybes
        [
            Just $ TBool
        ,   Just $ TSet  TAny
        ,   Just $ TMSet TAny
        ,   Just $ TPar  TAny
        -- ,   Just $ TRel (any number of anys  )
        ,   Just $ TFunc TAny TAny
        ]

    process :: Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]

    process fty@(TMSet _)  = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
              simple fty [ (EUniOp . UBar, 1)]
                +| d - (fromInteger $ depthOf fty) >= 1

            , do
                nty@(TMSet ins) <- purgeAny fty
                (nty,) <$> sequence [ freq ins ]
                    +| d - (fromInteger $ depthOf nty) >= 1

            ]

        freq :: PType -> GG (ToTypeFn, Depth)
        freq i =  do
            ep <- withDepthDec (exprOf i)
            return $ raise $ (EProc . (flip Pfreq) ep, 1)

    process fty@TBool = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
              simple fty [ (EProc . PtoInt, 1)]
                +| d - (fromInteger $ depthOf fty) >= 1

            ]


    process fty@(TSet _)    = onlyBar fty
    process fty@(TPar _)    = onlyBar fty
    process fty@(TRel _)    = onlyBar fty
    process fty@(TFunc _ _) = onlyBar fty
    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]

    onlyBar fty = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
              simple fty [ (EUniOp . UBar, 1)]
                +| d - (fromInteger $ depthOf fty) >= 1
            ]


reachableToType d oty@(TSet ity) =  join (ss oty) (concatMapM process (types))
    where


    ss (TSet (TSet _)) = reachableToTypeSetSet d oty
    ss _ = return []

    join a b = do
        aa <- a
        bb <- b
        return $ aa ++ bb

    types =  catMaybes [ Just  $  TFunc ity TAny
                       , Just  $  TFunc TAny ity
                       , TRel <$> tupleInner ity
                       , Just  $  TPar  ity
                       , Just  $  TMSet ity
                       , Just  $  TSet  ity
                       ]

    tupleInner :: Type -> Maybe [Type]
    tupleInner (TTuple ts) = Just ts
    tupleInner _           = Nothing

    process :: Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]
    process fty@(TSet _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        -- FIXME weighting
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                do
                nty <- purgeAny fty
                (nty,) <$>  sequence [ union nty, intersect nty, diff nty ]
                    +| d - (fromInteger $ depthOf nty) >= 1
            ]

    process fty@(TMSet _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
             simple fty [ (EProc . PtoSet, 1)]
                +| d - (fromInteger $ depthOf fty) >= 1
            ]


    process fty@(TPar _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
              simple fty [ (EProc . Pparticipants, 1)]
                +| d - (fromInteger $ depthOf fty) >= 1
            , do
                nty@(TPar ins) <- purgeAny fty
                (nty,) <$> sequence [ party ins ]
                    +| d - (fromInteger $ depthOf nty) >= 1

            ]

        party :: PType -> GG (ToTypeFn, Depth)
        party i =  do
            ep <- withDepthDec (exprOf i)
            return $ raise $ (EProc . Pparty ep, 1)


    process fty@(TRel _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
             simple fty [ (EProc . PtoSet, 1)]
                +| d - (fromInteger $ depthOf fty) >= 2
                -- one for toSet, one for the rel
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


reachableToType d oty@(TMSet ity) =  concatMapM process (types)
    where

    types =  catMaybes [ Just  $  TFunc ity TAny
                       , Just  $  TFunc TAny ity
                       , TRel <$> tupleInner ity
                       , Just  $  TMSet ity
                       , Just  $  TSet  ity
                       ]

    tupleInner :: Type -> Maybe [Type]
    tupleInner (TTuple ts) = Just ts
    tupleInner _           = Nothing

    process :: Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]
    process fty@(TMSet _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                do
                nty <- purgeAny fty
                (nty,) <$>  sequence [ union nty, intersect nty, diff nty ]
                    +| d - (fromInteger $ depthOf nty) >= 1
            ]

    process fty@(TSet _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
             simple fty [ (EProc . PtoMSet, 1)]
                +| d - (fromInteger $ depthOf fty) >= 1
            ]
            
            
    process fty@(TRel _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
             simple fty [ (EProc . PtoMSet, 1)]
                +| d - (fromInteger $ depthOf fty) >= 2
                -- one for toSet, one for the rel
            ]


    process fty@(TFunc a b) = funcs >>=
        concatMapM (uncurry (processCommon d) )

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence 
            -- 1 for func
            -- 1 usually for TFunc
            [
              simple fty [ (EProc . PtoMSet, 1)]
                +| d - (fromInteger $ max (depthOf a ) (depthOf b) ) > 2
                && typesUnify  (TTuple [a,b]) ity
            ]

    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]



reachableToType d oty@(TRel inners)   =  concatMapM process types

    where
    types =  catMaybes
        [
            TFunc (inners !! 0) (inners !! 1)  *| length inners == 2 &&  d >= 2
        ,   oty *|  d >= 3
        ]

    process :: Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]
    process fty@(TFunc _ _ ) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                simple fty [ (EProc . PtoRelation, 1)]
                    +| d - (fromInteger $ depthOf fty) >= 1
            ]

    process fty@(TRel _) | fty == oty = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                do
                nty <- purgeAny fty
                (nty,) <$>  sequence [ union nty, intersect nty, diff nty ]
                    +| d - (fromInteger $ depthOf nty) >= 1
            ]

    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]


reachableToType d oty@(TFunc _ _ ) = concatMapM process types
    where

    types =  catMaybes [ oty *| d >= 2
                       ]

    process :: Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]
    process fty@(TFunc _ _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                do
                nty <- purgeAny fty
                (nty,) <$>  sequence [ intersect nty, diff nty ]
                    +| d - (fromInteger $ depthOf nty) >= 1
            ]


    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]


reachableToType d oty@(TMatix TInt) = concatMapM process types
    where

    types =  catMaybes
        [
            TMSet TAny *| d >= 2
        ]

    process :: Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]

    process fty@(TMSet _)  = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
            do
            nty@(TMSet ins) <- purgeAny fty
            (nty,) <$> sequence [ hist ins ]
                +| d - (fromInteger $ depthOf nty) >= 1

            ]

        hist :: PType -> GG (ToTypeFn, Depth)
        hist i =  do
            -- matrix indexed by [w] of i
            ep <- withDepthDec (exprOf $ TMatix i)
            return $ raise $ (EProc . (flip Phist) ep, 1)

    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]


reachableToType _ (TMatix _ )     = return []
reachableToType _ (TTuple _ )     = return []
reachableToType _ (TPar _)        = return []
reachableToType _ (TUnamed _)     = return []
reachableToType _ (TEnum _)       = return []


reachableToTypeSetSet :: Depth -> Type -> GG [ (Type, GG [(ToTypeFn, Depth)] ) ]
reachableToTypeSetSet d oty@(TSet (TSet inner) ) = concatMapM process types
    where

    types =  catMaybes [ Just $ TPar inner ]

    process :: Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]
    process fty@(TPar _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
              simple fty [ (EProc . Pparts, 1)]
                  +| d - (fromInteger $ depthOf fty) >= 1
            ]

    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]


reachableToTypeSetSet d ty =
    ggError "reachableToTypeSetSet" [nn "d" d, nn "ty" ty ]


union :: PType -> GG (ToTypeFn, Depth)
union i = do
    other <- withDepthDec $ exprOf i
    ff <- elements2 [Bunion, flip Bunion]
    return $ raise $ (EBinOp . ff other, 1)

intersect :: PType -> GG (ToTypeFn, Depth)
intersect i = do
    other <- withDepthDec $ exprOf i
    ff <- elements2 [Bintersect, flip Bintersect]
    return $ raise $ (EBinOp . ff other, 1)

diff :: PType -> GG (ToTypeFn, Depth)
diff i = do
    other <- withDepthDec $ exprOf i
    ff <- elements2 [BDiff, flip BDiff]
    return $ raise $ (EBinOp . ff other, 1)



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
