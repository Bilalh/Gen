

{-# LANGUAGE MultiWayIf #-}

module Gen.Arbitrary.FromNested where

import Gen.Prelude
import Gen.Arbitrary.Expr
-- import Gen.Arbitrary.Type
import Gen.Arbitrary.Op

import qualified Data.Map as M
-- import Data.Maybe(isJust)


-- returns a Ref to var that be reached with the remaining depth
{-
    e.g. if want  Tint and we have a var
        find tu :  tuple( int, int )

    this could return depending on the depth
        tu[0]        # depth 2   (indexing counts as level)
        tu[ |{0}| ]  # depth 4

    e.g. TBool  from
        find f :  set of int -> bool

    this could return depending on the depth
        f( {1} )   # depth 4

-}

nestedVarsOf :: TType -> GG (Maybe (GG Expr))
-- nestedVarsOf  ty = return Nothing
nestedVarsOf ty = gets beConstant_ >>= \case
    True  -> return Nothing
    False -> nestedVarsOf' ty  -- TODO too strict?

nestedVarsOf' :: TType -> GG (Maybe (GG Expr))
nestedVarsOf' tyTo = do
    addLog "nestedVarsOf" []

    -- FIXME are types enough?
    doms     <- gets doms_
    quanVars <- gets newVars_
    let refs =  quanVars  ++ (toTy <$> M.toList doms)

    gets depth_ >>= \d -> case (d, refs) of
        (0, _) -> return Nothing
        (_, []) -> return Nothing
        (_, _) ->  do
            filterM ( \(_, from) ->
                (inDepth d ) <$> typeReachable from tyTo ) refs >>=
                 \case
                    [] -> return Nothing
                    xs -> do
                        tu@(name,tyFrom) <- elements2 xs
                        addLog "nestedVarsOf'gets" [pretty name, pretty tyFrom]
                        return . Just $  exprFromRefTo tu tyTo


    where
        inDepth _ Nothing  = False
        inDepth d (Just c) = c <  d

        toTy (ref, fg) = (ref, typeOfDom . domOfGF $ fg )

--TODO Make sure to handle Tany

-- Returns the min number of steps to convert types
typeReachable :: TType -> TType -> GG (Maybe Int)
typeReachable from to  | to == from =  return (Just 0)

typeReachable TBool TInt     = return (Just 1) -- ToInt(bool)
typeReachable (TSet _) TInt  = return (Just 1) -- |set|

-- val1 != val2
typeReachable _ TBool = return (Just 1)

-- using set literal  {val}
typeReachable from (TSet inner)  | from == inner = return (Just 1)

-- -- 1 + converting to the inner type
-- typeReachable (TSet inner) from = do
--     i <- typeReachable (inner) from
--     return $  (+1) <$> i

--  t: tuple(ty, ty2 )  ty  -> t[1]
typeReachable (TTuple inners) to = do
    case any (== to ) inners of
        True  -> return . Just $ 2
        False -> return Nothing

-- typeReachable (TMatix inner) _ = error "dd"

typeReachable (TMatix inner) to | inner == to = do
    d <- gets depth_
    addLog "typeReachable mat of" [pretty inner, "to" <+> pretty to
                                  , "depth_" <+> pretty d ]
    return . Just $ 2


-- FIXME  need domain for function?
--  f: function  int -> Bool
--  want a bool
--  we can do f(?int)
-- do we want to make sure ?int is inside the domain of f?
-- if so we need to use the domains as well

typeReachable (TFunc ffrom fto) to | fto == to  = return . Just . fromInteger $
    depthOf ffrom

typeReachable _ _ = return Nothing

-- typeReachable to from = ggError "typeReachable"
--     ["to" <+> pretty to
--     ,"from" <+> pretty from
--     ]


-- returns a expr that contraints the Ref
exprFromRefTo :: (Ref,TType) -> TType -> GG Expr
exprFromRefTo (ref,tyFrom) tyTo = do
    addLog "exprFromRefTo" []
    -- TODO inefficient
    minSteps <- typeReachable tyTo tyFrom

    d <- gets depth_
    addLog "exprFromRefTo" ["depth, ref, fromTy, tyTo"
                           ,pretty d,  pretty ref, pretty tyFrom, pretty tyTo  ]

    process (EVar ref) tyFrom


    where
    process cur from = do
        choices <- nextt cur from tyTo
        (newCur, newFrom ) <- elements2 choices
        if
            | newFrom == tyTo -> return newCur
            | otherwise ->  process newCur newFrom




-- A generator for all types which would still be able to reach
-- the destination


nextt, nextt' ::
    Expr -> -- current  (starts as just the ref)
    TType -> -- current's Type
    TType -> -- Final Destination type
    --TODO should be GG [ GG (Expr, Type) ] ?
    GG [ (Expr, TType) ] -- A list of possible transformations + their type

nextt cur tyFrom tyTo  = gets depth_ >>= \d -> if
    | d < 0 -> ggError "nextt depth <0 " $ ["cur tyFrom tyTo"
                                           , pretty cur
                                           , pretty tyFrom
                                           , pretty tyTo ]

    | otherwise -> nextt' cur tyFrom tyTo

nextt' cur tyFrom tyTo | tyFrom == tyTo = return [ (cur, tyTo) ]
nextt' cur tyFrom tyTo = do
    addLog "next'" [pretty tyFrom, pretty tyTo]

    ff tyFrom tyTo

    where
    -- :: from to -> choices
    ff :: TType -> TType -> GG [(Expr, TType)]
    ff TBool TInt  = do
        return [ ( EProc $ PtoInt cur , TInt)  ]

    ff (TSet _) TInt = do
        return [( EUniOp $ UBar cur , TInt) ]

    ff ty TBool = do
        op <- boolOpFor ty
        return $ [(op cur cur, TBool)]

    ff from to@(TSet inner) | from == inner  = do
        return [(ELit $ ESet $ [EExpr cur], to )]

    ff (TTuple inners) to | any (==to) inners = do
        let withIdx = zip inners [1..]
            possible =  filter (\(f,_) -> f == to ) withIdx

        (_, cIndex) <- elements2 possible
        return $ [  (EProc $ Pindex cur (ELit $ EI cIndex)  , to)  ]

    ff (TFunc ffrom fto) to | fto == to = do
        indexer <-  exprOf ffrom
        return [ (EProc $ Papply cur [indexer] , to) ]

    ff (TMatix inner) to | inner == to = do
        addLog "ffmat" []
        indexer <- exprOf TInt
        return $ [ (EProc $ Pindex cur (indexer ), to) ]


    ff from to = ggError "ff unmatched" $ [  "cur" <+> pretty cur
                                          , "tyFrom" <+> pretty from
                                          , "tyTo" <+> pretty to ]
