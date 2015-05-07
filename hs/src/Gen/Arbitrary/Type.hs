module Gen.Arbitrary.Type where

import Gen.Arbitrary.Prelude

-- return the type of a, knowing b  from  `a in b`
quanType_in ::Type ->Type
quanType_in (TypeSet inner) = inner


atype_only :: [Type] -> GG Type
atype_only tys = do
    addLog "atype_only" ["start"]

    d <- gets depth_
    if | d < 0  -> ggError "atype_only invalid depth" []
       | otherwise -> do

        let inDepth = filter (\t -> (fromInteger $ depthOf t) <= d ) tys
        choice <- oneof2 (map converted inDepth)
        return choice

    where
        converted ::Type -> GG Type
        converted (TypeMatrix x TypeAny)  = liftM (TypeMatrix x) (withDepthDec atype)
        converted (TypeSet TypeAny)    = liftM TypeSet (withDepthDec atype)
        converted (TypeMSet TypeAny)   = liftM TypeMSet (withDepthDec atype)
        converted (TypePartition TypeAny)    = liftM TypePartition (withDepthDec atype)

        converted (TypeFunction TypeAny TypeAny) = return TypeFunction
                    `ap`  (withDepthDec atype)
                    `ap`  (withDepthDec atype)

        converted (TypeFunction TypeAny b) = return TypeFunction
                    `ap`  (withDepthDec atype)
                    `ap`  (return b)

        converted (TypeFunction a TypeAny) = return TypeFunction
                    `ap`  (return a)
                    `ap`  (withDepthDec atype)

        converted (TypeTuple [TypeAny]) = atuple
        converted (TypeRelation   [TypeAny]) = arel
        converted ty = return ty


atype_def :: GG Type
atype_def = do
    addLog "atype_def" ["start"]
    d <- gets depth_
    addLog "atype_def" ["depth_" <+> pretty d]


    res <- if
        | d < 0  -> ggError "atype_def invalid depth" []
        | d == 0 -> elements2 [TypeBool, TypeInt]
        | d == 1 -> do
            let inner = withDepth 0
            oneof2 [
                  elements2 [TypeBool, TypeInt]
                -- , liftM TypeMatrix (inner atype_def)
                , liftM TypeSet  (inner atype_def)
                , liftM TypeMSet (inner atype_def)
                , liftM TypePartition  (inner atype_def)
                , return TypeFunction
                    `ap`  (inner atype_def)
                    `ap`  (inner atype_def)
                , atuple
                ]

        | otherwise -> do
            let inner = withDepth (d - 1)
            oneof2 [
                  elements2 [TypeBool, TypeInt]
                , liftM (TypeMatrix TypeInt) (inner atype_def)
                , liftM TypeSet  (inner atype_def)
                , liftM TypeMSet (inner atype_def)
                , liftM TypePartition  (inner atype_def)
                , return TypeFunction
                    `ap`  (inner atype_def)
                    `ap`  (inner atype_def)
                , atuple
                , arel
                ]
    d' <- gets depth_
    addLog "atype_def" ["resTy" <+> pretty res, "depth_" <+> pretty d' ]
    return res

atuple :: GG Type
atuple = do
    depth_ <- gets depth_
    addLog "atuple" ["depth_" <+> pretty depth_]

    vs <- listOfBounds (1,  min 10 (2 * depth_))
        (withDepth (depth_ - 1) atype_def )
    return $ TypeTuple vs

-- a relation e.g   relation (  tuple(int,int) )
-- has a nesting of 2  int -> tuple -> relation

arel :: GG Type
arel = do

    d <- gets depth_
    addLog "arel" ["depth_" <+> pretty d]

    vs <- listOfBounds (1,  min 5 (2 * d))
        (withDepth (d - 2) atype_def )

    return $ TypeRelation vs
