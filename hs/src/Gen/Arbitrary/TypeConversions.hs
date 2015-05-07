{-# LANGUAGE QuasiQuotes, TupleSections #-}

module Gen.Arbitrary.TypeConversions(toTypeWithConversions) where

import Gen.Arbitrary.Expr
import Gen.AST.Ops
import Gen.Arbitrary.Prelude

--Type that does not have any anys
type PType =Type

-- for ghci usage
_aa ::Type ->  GG (Maybe Expr)
_aa ty = do
    toTypeWithConversions ty >>= \case
        Nothing -> return Nothing
        Just xs -> do
            xx <- xs
            return $ Just xx


toTypeWithConversions ::Type -> GG (Maybe (GG Expr))
toTypeWithConversions ty = do
    d <- gets depth_
    addLog "toType" ["depth_" <+> pretty d, "ty" <+> pretty ty]

    -- Simple cases
    if
        | d < 0  -> ggError "toTypeWithConversions depth_ <0" ["ty:" <+> pretty ty]
        | d == 0 ->  return Nothing
        | otherwise  -> con ty


con ::Type -> GG (Maybe (GG Expr))
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
---- e.g  TypeInt  ->  Just $  [TypeSet Tint,  [  | x |, 1  ]   ]

-- return (the type, (function applied and depth needed)  )

reachableToTypeWithCommon :: Depth ->Type -> GG [ (Type, GG [(ToTypeFn, Depth)] ) ]
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
                nty <- TypeSet <$> purgeAny y
                customM nty [minn **| useFunc Amin , maxx **| useFunc Amax]
                    ++| d - (fromInteger $ depthOf nty) >= 2
            ,   do
                nty <- TypeMSet <$> purgeAny y
                customM nty [minn **| useFunc Amin , maxx **| useFunc Amax]
                    ++| d - (fromInteger $ depthOf nty) >= 2
            ]

    minn, maxx :: GG (ToTypeFn, Depth)
    minn = return $ raise $ (opMin, 1)
    maxx = return $ raise $ (opMax, 1)



reachableToType :: Depth ->Type -> GG [ (Type, GG [(ToTypeFn, Depth)] ) ]
reachableToType 0 _ = return  []

--
-- reachableToType d TypeAny = do
--     newTy <- withSameDepth atype
--     reachableToType d newTy

reachableToType d oty@TypeBool = do
    addLog "reachableToType" ["TypeBool" ]
    concatMapM process types
    where

    types =  catMaybes
        [
            TypePartition TypeAny       *| d >= 2
        ,   TypeAny            *| d >= 2
        ,   TypeFunction TypeAny TypeAny *| d >= 2
        ]

    process ::Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]
    process TypeAny = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                do
                innerTy <- withDepth (d - 2) atype
                container <- elements2 [TypeSet, TypeMSet ]
                --TODO  could also allow the other way around?
                customM innerTy [ element (container innerTy) **| useFunc Aelement ]
                    ++| True

            ]

        element :: PType -> GG (ToTypeFn, Depth)
        element i = do
            e1 <- withDepthDec $ exprOf i
            return $ raise $ ( (flip opIn) e1, 1)

    process (TypeFunction _ _ )  = return []
    -- process fty@(TypeFunction _ _ ) = funcs >>=
    --     concatMapM (uncurry (processCommon d))

    --     where
    --     funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
    --     funcs = catMaybes <$> sequence
    --         [
    --         do
    --         (TypeFunction a b) <- withDepthDec (purgeAny fty)

    --         op <-  elements2 [ Pinverse , flip Pinverse  ]
    --         (t1,t2) <- elements2 [ (TypeFunction a b, TypeFunction b a ),  (TypeFunction b a, TypeFunction a b ) ]
    --         e1 <- withDepthDec (exprOf t1)

    --         customM t2  [ (return . raise) (op e1, 1) **| useFunc Ainverse ]
    --             ++| d - (max (fromInteger $ depthOf t1 ) (fromInteger $ depthOf t2 )) > 1
    --         ]


    process fty@(TypePartition _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                do
                nty@(TypePartition inn) <- purgeAny fty
                customM nty [ together inn **| useFunc Atogether , apart inn **| useFunc Aapart  ]
                    ++| d - (fromInteger $ depthOf nty) >= 1

            ]

        together :: PType -> GG (ToTypeFn, Depth)
        together i = do
            e1 <- withDepthDec $ exprOf i
            e2 <- withDepthDec $ exprOf i
            return $ raise $ (opTogether e1 e2, 1)

        apart :: PType -> GG (ToTypeFn, Depth)
        apart i = do
            e1 <- withDepthDec $ exprOf i
            e2 <- withDepthDec $ exprOf i
            return $ raise $ (opApart e1 e2, 1)

    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]


reachableToType d oty@TypeInt = concatMapM process types
    where

    types =  catMaybes
        [
            Just $ TypeBool
        ,   Just $ TypeSet  TypeAny
        ,   Just $ TypeMSet TypeAny
        ,   Just $ TypePartition  TypeAny
        -- ,   Just $ TypeRelation (any number of anys  )
        ,   Just $ TypeFunction TypeAny TypeAny
        ]

    process ::Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]

    process fty@(TypeMSet _)  = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
              simpleM fty [ (opBar, 1) **| useFunc Aubar ]
                ++| d - (fromInteger $ depthOf fty) >= 1

            , do
                nty@(TypeMSet ins) <- purgeAny fty
                customM nty [ freq ins **| useFunc Afreq ]
                    ++| d - (fromInteger $ depthOf nty) >= 1

            ]

        freq :: PType -> GG (ToTypeFn, Depth)
        freq i =  do
            ep <- withDepthDec (exprOf i)
            return $ raise $ ( (flip opFreq) ep, 1)

    process fty@TypeBool = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
              simpleM fty [ (opToInt, 1) **| useFunc AtoInt  ]
                ++| d - (fromInteger $ depthOf fty) >= 1

            ]


    process fty@(TypeSet _)    = onlyBar fty
    process fty@(TypePartition _)    = onlyBar fty
    process fty@(TypeRelation _)    = onlyBar fty
    process fty@(TypeFunction _ _) = onlyBar fty
    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]

    onlyBar fty = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
              simpleM fty [ (opBar, 1) **| useFunc Aubar ]
                ++| d - (fromInteger $ depthOf fty) >= 1
            ]


reachableToType d oty@(TypeSet ity) =  do
    addLog "reachToTy" [nn "depth" d, nn "ty" oty ]
    join (ss oty) (concatMapM process (types))
    where


    ss (TypeSet (TypeSet _)) = reachableToTypeSetSet d oty
    ss _ = return []

    join a b = do
        aa <- a
        bb <- b
        return $ aa ++ bb

    types =  catMaybes [ Just  $  TypeFunction ity TypeAny
                       , Just  $  TypeFunction TypeAny ity
                       , TypeRelation <$> tupleInner ity
                       , Just  $  TypePartition  ity
                       , Just  $  TypeMSet ity
                       , Just  $  TypeSet  ity
                       ]

    tupleInner ::Type -> Maybe [Type]
    tupleInner (TypeTuple ts) = Just ts
    tupleInner _           = Nothing

    process ::Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]
    process fty@(TypeSet _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                do
                nty <- purgeAny fty
                addLog "reachToTy:set:set"
                    [ nn "d" d
                    , nn "nty" nty
                    , nn "depth nty" (depthOf nty)
                    , nn "con" (d - (fromInteger $ depthOf nty) >= 1)]
                customM nty [ union nty **| useFunc Aunion , intersectG nty **| useFunc Aintersect
                        , diff nty **| useFunc Adiff ]
                    ++| d - (fromInteger $ depthOf nty) >= 1
            ]

    process fty@(TypeMSet _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
             simpleM fty [ (opToSet, 1) **| useFunc AtoSet ]
                ++| d - (fromInteger $ depthOf fty) >= 1
            ]


    process fty@(TypePartition _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
              simpleM fty [ (opParticipants, 1) **| useFunc Aparticipants ]
                ++| d - (fromInteger $ depthOf fty) >= 1
            , do
                nty@(TypePartition ins) <- purgeAny fty
                customM nty [ party ins  **| useFunc Aparty ]
                    ++| d - (fromInteger $ depthOf nty) >= 1

            ]

        party :: PType -> GG (ToTypeFn, Depth)
        party i =  do
            ep <- withDepthDec (exprOf i)
            return $ raise $ (opParty ep, 1)


    process fty@(TypeRelation _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
             simpleM fty [ (opToSet, 1) **| useFunc AtoSet ]
                ++| d - (fromInteger $ depthOf fty) >= 2
                -- one for toSet, one for the rel
            ]


    process fty@(TypeFunction a b) = funcs >>=
        concatMapM (uncurry (processCommon d) )

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            -- 1 for func
            -- 1 usually for TypeFunction

            [ simpleM fty [ (opDefined, 1) **| useFunc Adefined ]
                ++| a == ity &&  d - (fromInteger $ depthOf a) > 2

            , simpleM fty [  (opRange, 1)  **| useFunc Arange  ]
                ++| b == ity && d - (fromInteger $ depthOf b) > 2


            ,simpleM fty [ (opToSet, 1) **| useFunc AtoSet ]
                ++| d - (fromInteger $ max (depthOf a ) (depthOf b) ) > 2
                && typesUnify  [TypeTuple [a,b], ity]

            ,do
                nb <- purgeAny b
                customM  (TypeFunction a nb) [ preImage nb **| useFunc ApreImage ]
                    ++| a == ity
                    && d - (fromInteger $ max (depthOf a ) (depthOf nb) ) > 2
            -- ,do
                -- na <- purgeAny a
                -- customM (TypeFunction na b ) [image na  **| useFunc Aimage ]
                --     ++| b == ity
                --     && d - (fromInteger $ max (depthOf na ) (depthOf b) ) > 2

            ]


        preImage :: PType -> GG (ToTypeFn, Depth)
        preImage pb = do
            ep <- withDepthDec (exprOf pb)
            return $ raise $ ((flip opPreImage) ep, 1)

        -- Other image `does not exist`
        -- image :: PType -> GG (ToTypeFn, Depth)
        -- image pa = do
        --     ep <- withDepthDec (exprOf $ TypeSet pa)
        --     return $ raise $ (EProc . Pimage ep, 1)

    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]


reachableToType d oty@(TypeMSet ity) =  concatMapM process (types)
    where

    types =  catMaybes [ Just  $  TypeFunction ity TypeAny
                       , Just  $  TypeFunction TypeAny ity
                       , TypeRelation <$> tupleInner ity
                       , Just  $  TypeMSet ity
                       , Just  $  TypeSet  ity
                       ]

    tupleInner ::Type -> Maybe [Type]
    tupleInner (TypeTuple ts) = Just ts
    tupleInner _           = Nothing

    process ::Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]
    process fty@(TypeMSet _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                do
                nty <- purgeAny fty
                customM nty [ union nty **| useFunc Aunion , intersectG nty **| useFunc Aintersect
                            , diff nty  **| useFunc Adiff ]
                    ++| d - (fromInteger $ depthOf nty) >= 1
            ]

    process fty@(TypeSet _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
             simpleM fty [ (opToMSet, 1) **| useFunc AtoMSet ]
                ++| d - (fromInteger $ depthOf fty) >= 1
            ]


    process fty@(TypeRelation _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
             simpleM fty [ (opToMSet, 1) **| useFunc AtoMSet  ]
                ++| d - (fromInteger $ depthOf fty) >= 2
                -- one for toSet, one for the rel
            ]


    process fty@(TypeFunction a b) = funcs >>=
        concatMapM (uncurry (processCommon d) )

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            -- 1 for func
            -- 1 usually for TypeFunction
            [
              simpleM fty [ (opToMSet, 1) **| useFunc AtoMSet ]
                ++| d - (fromInteger $ max (depthOf a ) (depthOf b) ) > 2
                && typesUnify  [TypeTuple [a,b], ity]
            ]

    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]



reachableToType d oty@(TypeRelation inners)   =  concatMapM process types

    where
    types =  catMaybes
        [
            TypeFunction (inners `at` 0) (inners `at` 1)  *| length inners == 2 &&  d >= 2
        ,   oty *|  d >= 3
        ]

    process ::Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]
    process fty@(TypeFunction _ _ ) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                simpleM fty [ (opToRelation, 1) **| useFunc AtoRelation ]
                    ++| d - (fromInteger $ depthOf fty) >= 1
            ]

    process fty@(TypeRelation _) | fty == oty = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                do
                nty <- purgeAny fty
                customM nty [ union nty **| useFunc Aunion , intersectG nty **| useFunc Aintersect
                            , diff nty  **| useFunc Adiff ]
                    ++| d - (fromInteger $ depthOf nty) >= 1
            ]

    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]


reachableToType d oty@(TypeFunction _ _ ) = concatMapM process types
    where

    types =  catMaybes [ oty *| d >= 2
                       ]

    process ::Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]
    process fty@(TypeFunction _ _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
                do
                nty <- purgeAny fty
                customM nty [intersectG nty **| useFunc Aintersect
                            , diff nty     **| useFunc Adiff ]
                    ++| d - (fromInteger $ depthOf nty) >= 1
            ]


    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]


reachableToType d oty@(TypeMatrix _ TypeInt) = concatMapM process types
    where

    types =  catMaybes
        [
            TypeMSet TypeAny *| d >= 2
        ]

    process ::Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]

    process fty@(TypeMSet _)  = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
            do
            nty@(TypeMSet ins) <- purgeAny fty
            customM nty [ hist ins **| useFunc Ahist ]
                ++| d - (fromInteger $ depthOf nty) >= 1

            ]

        hist :: PType -> GG (ToTypeFn, Depth)
        hist _ =  do
            return $ raise $ (opHist, 1)

    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]


reachableToType _ (TypeMatrix _ _ ) = return []
reachableToType _ (TypeTuple _ )    = return []
reachableToType _ (TypePartition _) = return []
reachableToType _ (TypeUnnamed _)   = return []
reachableToType _ (TypeEnum _)      = return []


reachableToTypeSetSet :: Depth ->Type -> GG [ (Type, GG [(ToTypeFn, Depth)] ) ]
reachableToTypeSetSet d oty@(TypeSet (TypeSet inner) ) = concatMapM process types
    where

    types =  catMaybes [ Just $ TypePartition inner ]

    process ::Type -> GG [ (Type, GG [(ToTypeFn,Depth)] ) ]
    process fty@(TypePartition _) = funcs >>=
        concatMapM (uncurry (processCommon d))

        where
        funcs :: GG [ (Type, [(ToTypeFn, Depth)]) ]
        funcs = catMaybes <$> sequence
            [
              simpleM fty [ (opParts, 1) **| useFunc Aparts ]
                  ++| d - (fromInteger $ depthOf fty) >= 1
            ]

    process ty = ggError "reachableToType missing"
        ["ty" <+> pretty ty, "oty" <+> pretty oty ]


reachableToTypeSetSet d ty =
    ggError "reachableToTypeSetSet" [nn "d" d, nn "ty" ty ]


union :: PType -> GG (ToTypeFn, Depth)
union i = do
    other <- withDepthDec $ exprOf i
    ff <- elements2 [opUnion, flip opUnion]
    return $ raise $ (ff other, 1)

intersectG :: PType -> GG (ToTypeFn, Depth)
intersectG i = do
    other <- withDepthDec $ exprOf i
    ff <- elements2 [opIntersect, flip opIntersect]
    return $ raise $ (ff other, 1)

diff :: PType -> GG (ToTypeFn, Depth)
diff i = do
    other <- withDepthDec $ exprOf i
    ff <- elements2 [opMinus, flip opMinus]
    return $ raise $ (ff other, 1)



processCommon :: Depth -> Type -> [(ToTypeFn,Depth)] -> GG [(Type, GG [(ToTypeFn,Depth)] ) ]
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

infixl 1 **|

(**|) :: a -> GG Bool -> GG (Maybe a)
a  **| c  = do
    b <- c
    if b then
        return (Just a)
    else
        return Nothing


simpleM :: (Functor f, Monad f)
  =>  t -> [f (Maybe (Expr -> Expr, Depth))]
  ->  f (Maybe  (t, [(ToTypeFn, Depth)]) )
simpleM ty vsM= do
    vs <- sequence vsM
    simpleM' ty (catMaybes vs)

    where
    simpleM' _  []  = return Nothing
    simpleM' tyy val = (tyy,) <$>  mapM ( return . raise) val >>= return . Just

customM :: (Functor f, Monad f)
  =>  t -> [f (  Maybe (f (ToTypeFn, Depth)) ) ]
  ->  f (Maybe  (t, [(ToTypeFn, Depth)]) )
customM ty vsM= do
    vs <- sequence vsM
    customM' ty (catMaybes vs)

    where
    customM' _  []  = return Nothing
    customM' tyy val = (tyy,) <$>  sequence  val >>= return . Just



-- customM0 :: (Functor f, Monad f)
--   =>  t -> [Maybe (f (ToTypeFn, Depth)) ]
--   ->  f (Maybe  (t, [(ToTypeFn, Depth)]) )
-- customM0 ty val= customM' ty (catMaybes val)

--     where
--     customM' _  []  = return Nothing
--     customM' ty val = (ty,) <$>  sequence  val >>= return . Just

-- simpleM0 :: (Functor f, Monad f)
--   =>  t -> [Maybe (Expr -> Expr, Depth)]
--   ->  f (Maybe  (t, [(ToTypeFn, Depth)]) )
-- simpleM0 ty val=  simpleM' ty (catMaybes val)

--     where
--     simpleM' _  []  = return Nothing
--     simpleM' ty val = (ty,) <$>  mapM ( return . raise) val >>= return . Just


infixl 1 ++|
(++|) :: Monad m =>  m (Maybe a) -> Bool -> m (Maybe a)
_   ++| False =  return Nothing
mxs ++| _ = do
    xs <- mxs
    case xs of
        Just xx -> return (Just xx)
        Nothing -> return Nothing
