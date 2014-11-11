{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, ScopedTypeVariables#-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE LambdaCase, MultiWayIf, TemplateHaskell #-}

module TestGen.Arbitrary.TypeConversions where

import TestGen.Arbitrary.Helpers.Prelude
import TestGen.Arbitrary.Expr
import TestGen.Arbitrary.Literal



toTypeWithConversions :: Type -> GG (Maybe ( Expr))
toTypeWithConversions ty = do
    d <- gets depth_

    -- Simple cases
    if
        | d < 0 -> ggError "exprOf depth_ <0" ["exprTy:" <+> pretty ty]
        | d == 0 ->  return Nothing
        | otherwise  -> con d ty


con :: Depth -> Type -> GG (Maybe (Expr))
con _ to  = do
    d <- gets depth_
    mchoices <- reachableToType d to

    case mchoices of
        Nothing -> return Nothing
        Just choices -> do
            (fromTy, toFn) <- elements2 choices
            fromExpr <- withDepthDec (exprOf fromTy)
            fs <- toFn
            f <- elements2 fs
            return $  Just $ f fromExpr



type ToTypeFn = (Expr -> Expr)

-- Give an type return the possible ways to get to that type
-- e.g  TInt  ->  Just $  [TSet Tint,  [  | x |  ]   ]

reachableToType :: Depth -> Type -> GG (Maybe [ (Type, GG [ToTypeFn] )  ])
reachableToType 0 _ =return  Nothing
reachableToType d ( TSet (TTuple [a, b] )) = return $ Just $ [ (TFunc a b,
             return $  [ EProc . PtoSet ]
        )]

reachableToType d TBool = return Nothing


reachableToType d TInt = return $ Just $ [ -- (TBool,
        --     do
        --         let choices =  concatMap snd <$> reachableToType (d-1) TBool
        --         let arr = [ EProc . PtoInt ]
        --         arr ++ff arr choices
        -- ),
            (TSet (TTuple [TInt, TInt] ) ,
            do
                -- choices <- concatMap snd <$> reachableToType (d-1) (TSet (TTuple [TInt, TInt] ))
                choices <-  reachableToType (d-1) (TSet (TTuple [TInt, TInt] ))
                --FIXME need these types
                let choices2 = fmap (map snd) choices

                let arr = [ EUniOp . UBar ]
                a <-  combine arr choices2
                return $ arr ++ a
        )
    ]

combine :: [Expr -> Expr] -> Maybe [GG [ToTypeFn]] -> GG [ToTypeFn]
combine fs Nothing = return fs
combine xs (Just fsinners) = do
    f <- oneof2 fsinners
    x <- elements2 xs
    return $ map (\i ->  x . i )  f



sd :: GG ( Maybe ( Expr))
sd = return $ Just undefined

aa :: GG Expr
aa = do
    d <- map return <$> maybeToList <$> sd
    oneof2 $ d ++ [  boolLit ]
