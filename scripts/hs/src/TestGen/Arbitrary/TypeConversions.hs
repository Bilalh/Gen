{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, ScopedTypeVariables#-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE LambdaCase, MultiWayIf, TemplateHaskell, TupleSections #-}

module TestGen.Arbitrary.TypeConversions(toTypeWithConversions) where

import TestGen.Prelude
import TestGen.Arbitrary.Expr
import TestGen.Arbitrary.Literal
import TestGen.Arbitrary.Type

useFunc :: FuncsNames -> Bool
useFunc AtoInt = True 
useFunc _      = False 


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
                customM nty [min *| useFunc Amin , max *| useFunc Amax]
                    ++| d - (fromInteger $ depthOf nty) >= 2
                    
            ,   do
                nty <- TMSet <$> purgeAny y
                customM nty [min *| useFunc Amin , max *| useFunc Amax]
                    ++| d - (fromInteger $ depthOf nty) >= 2
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
                --TODO  could also allow the other way around?
                customM innerTy [ element (container innerTy) *| useFunc Aelement ]
                    ++| True

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

            customM t2  [ (return . raise) (EProc . op e1, 1) *| useFunc Ainverse ]
                ++| d - (max (fromInteger $ depthOf t1 ) (fromInteger $ depthOf t2 )) > 1
            ]


    process fty@(TPar _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                do
                nty@(TPar inn) <- purgeAny fty
                customM nty [ together inn *| useFunc Atogether , apart inn *| useFunc Aapart  ]
                    ++| d - (fromInteger $ depthOf nty) >= 1

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
              simpleM fty [ (EUniOp . UBar, 1) *| useFunc Aubar ]
                ++| d - (fromInteger $ depthOf fty) >= 1

            , do
                nty@(TMSet ins) <- purgeAny fty
                customM nty [ freq ins *| useFunc Afreq ]
                    ++| d - (fromInteger $ depthOf nty) >= 1

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
              simpleM fty [ (EProc . PtoInt, 1) *| useFunc AtoInt  ]
                ++| d - (fromInteger $ depthOf fty) >= 1

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
              simpleM fty [ (EUniOp . UBar, 1) *| useFunc Aubar ]
                ++| d - (fromInteger $ depthOf fty) >= 1
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
                customM nty [ union nty *| useFunc Aunion , intersect nty *| useFunc Aintersect
                        , diff nty *| useFunc Adiff ]
                    ++| d - (fromInteger $ depthOf nty) >= 1
            ]

    process fty@(TMSet _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
             simpleM fty [ (EProc . PtoSet, 1) *| useFunc AtoSet ]
                ++| d - (fromInteger $ depthOf fty) >= 1
            ]


    process fty@(TPar _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
              simpleM fty [ (EProc . Pparticipants, 1) *| useFunc Aparticipants ]
                ++| d - (fromInteger $ depthOf fty) >= 1
            , do
                nty@(TPar ins) <- purgeAny fty
                customM nty [ party ins  *| useFunc Aparty ]
                    ++| d - (fromInteger $ depthOf nty) >= 1

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
             simpleM fty [ (EProc . PtoSet, 1) *| useFunc AtoSet ]
                ++| d - (fromInteger $ depthOf fty) >= 2
                -- one for toSet, one for the rel
            ]


    process fty@(TFunc a b) = funcs >>=
        concatMapM (uncurry (processCommon d) )

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            -- 1 for func
            -- 1 usually for TFunc

            [ simpleM fty [ (EProc . Pdefined, 1) *| useFunc Adefined ]
                ++| a == ity &&  d - (fromInteger $ depthOf a) > 2

            , simpleM fty [  (EProc . Prange, 1)  *| useFunc Arange  ]
                ++| b == ity && d - (fromInteger $ depthOf b) > 2


            ,simpleM fty [ (EProc . PtoSet, 1) *| useFunc AtoSet ]
                ++| d - (fromInteger $ max (depthOf a ) (depthOf b) ) > 2
                && typesUnify  (TTuple [a,b]) ity

            ,do
                nb <- purgeAny b
                customM  (TFunc a nb) [ preImage nb *| useFunc ApreImage ]
                    ++| a == ity
                    && d - (fromInteger $ max (depthOf a ) (depthOf nb) ) > 2
            ,do
                na <- purgeAny a
                customM (TFunc na b ) [image na  *| useFunc Aimage ]
                    ++| b == ity
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
                customM nty [ union nty *| useFunc Aunion , intersect nty *| useFunc Aintersect
                            , diff nty *| useFunc Adiff ]
                    ++| d - (fromInteger $ depthOf nty) >= 1
            ]

    process fty@(TSet _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
             simpleM fty [ (EProc . PtoMSet, 1) *| useFunc AtoMSet ]
                ++| d - (fromInteger $ depthOf fty) >= 1
            ]
            
            
    process fty@(TRel _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
             simpleM fty [ (EProc . PtoMSet, 1) *| useFunc AtoMSet  ]
                ++| d - (fromInteger $ depthOf fty) >= 2
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
              simpleM fty [ (EProc . PtoMSet, 1) *| useFunc AtoMSet ]
                ++| d - (fromInteger $ max (depthOf a ) (depthOf b) ) > 2
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
                simpleM fty [ (EProc . PtoRelation, 1) *| useFunc AtoRelation ]
                    ++| d - (fromInteger $ depthOf fty) >= 1
            ]

    process fty@(TRel _) | fty == oty = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                do
                nty <- purgeAny fty
                customM nty [ union nty *| useFunc Aunion , intersect nty *| useFunc Aintersect
                            , diff nty *| useFunc Adiff ]
                    ++| d - (fromInteger $ depthOf nty) >= 1
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
                customM nty [intersect nty *| useFunc Aintersect 
                        , diff nty *| useFunc Adiff ]
                    ++| d - (fromInteger $ depthOf nty) >= 1
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
            customM nty [ hist ins *| useFunc Ahist ]
                ++| d - (fromInteger $ depthOf nty) >= 1

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
              simpleM fty [ (EProc . Pparts, 1) *| useFunc Aparts ]
                  ++| d - (fromInteger $ depthOf fty) >= 1
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



simpleM :: (Functor f, Monad f)
  =>  t -> [Maybe (Expr -> Expr, Depth)]
  ->  f (Maybe  (t, [(ToTypeFn, Depth)]) )
simpleM ty val=  simpleM' ty (catMaybes val)
     
    where
    simpleM' _  []  = return Nothing
    simpleM' ty val = (ty,) <$>  mapM ( return . raise) val >>= return . Just

customM :: (Functor f, Monad f)
  =>  t -> [Maybe (f (ToTypeFn, Depth)) ]
  ->  f (Maybe  (t, [(ToTypeFn, Depth)]) )
customM ty val= customM' ty (catMaybes val)
    
    where
    customM' _  []  = return Nothing
    customM' ty val = (ty,) <$>  sequence  val >>= return . Just    
    

infixl 1 ++|
(++|) :: Monad m =>  m (Maybe a) -> Bool -> m (Maybe a)
mxs ++| c = do
    xs <- mxs
    case (c, xs) of 
        (False, _)       -> return Nothing
        (True, Just xs ) -> return (Just xs)
        _                -> return Nothing

