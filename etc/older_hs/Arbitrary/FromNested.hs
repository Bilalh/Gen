module Gen.Arbitrary.FromNested where

import Conjure.Language.AbstractLiteral
import Conjure.Language.Constant
import Gen.Arbitrary.Expr
import Gen.Arbitrary.Op
import Gen.Arbitrary.Prelude
import Gen.AST.Ops

import qualified Data.Map as M


-- returns a Ref to var that be reached with the remaining depth
{-
    e.g. if want  Tint and we have a var
        find tu :  tuple( int, int )

    this could return depending on the depth
        tu[0]        # depth 2   (indexing counts as level)
        tu[ |{0}| ]  # depth 4

    e.g. TypeBool  from
        find f :  set of int -> bool

    this could return depending on the depth
        f( {1} )   # depth 4

-}

nestedVarsOf ::Type -> GG (Maybe (GG Expr))
-- nestedVarsOf  ty = return Nothing
nestedVarsOf ty = gets beConstant_ >>= \case
    True  -> return Nothing
    False -> nestedVarsOf' ty  -- TO DO too strict?

nestedVarsOf' ::Type -> GG (Maybe (GG Expr))
nestedVarsOf' tyTo = do
    addLog "nestedVarsOf" []

    -- FIX ME are types enough?
    doms     <- gets doms_
    quanVars <- gets newVars_
    let refs :: [Var] =  quanVars  ++ (map toVar . M.toList $ doms)

    gets depth_ >>= \d -> case (d, refs) of
        (0, _)  -> return Nothing
        (_, []) -> return Nothing
        (_, _) ->  do
            filterM ( \(Var _ from) ->
                (inDepth d ) <$> typeReachable from tyTo ) refs >>=
                 \case
                    [] -> return Nothing
                    xs -> do
                        tu :: Var <- elements2 xs
                        addLog "nestedVarsOf'gets" [nn "var" (groom tu)]
                        return . Just $  exprFromRefTo tu tyTo


    where
        inDepth _ Nothing  = False
        inDepth d (Just c) = c <  d

        toVar (ref, fg) = (Var ref (typeOfDom . domOfGF $ fg) )

--TO DO Make sure to handle Tany

-- Returns the min number of steps to convert types
typeReachable ::Type ->Type -> GG (Maybe Int)
typeReachable from to  | to == from =  return (Just 0)

typeReachable TypeBool TypeInt     = return (Just 1) -- ToInt(bool)
typeReachable (TypeSet _) TypeInt  = return (Just 1) -- |set|

-- val1 != val2
typeReachable _ TypeBool = return (Just 1)

-- using set literal  {val}
typeReachable from (TypeSet inner)  | from == inner = return (Just 1)

-- -- 1 + converting to the inner type
-- typeReachable (TypeSet inner) from = do
--     i <- typeReachable (inner) from
--     return $  (+1) <$> i

--  t: tuple(ty, ty2 )  ty  -> t[1]
typeReachable (TypeTuple inners) to = do
    case any (== to ) inners of
        True  -> return . Just $ 2
        False -> return Nothing

-- typeReachable (TypeMatrix inner) _ = error "dd"

typeReachable (TypeMatrix _ inner) to | inner == to = do
    d <- gets depth_
    addLog "typeReachable mat of" [pretty inner, "to" <+> pretty to
                                  , "depth_" <+> pretty d ]
    return . Just $ 2


-- FIX ME  need domain for function?
--  f: function  int -> Bool
--  want a bool
--  we can do f(?int)
-- do we want to make sure ?int is inside the domain of f?
-- if so we need to use the domains as well

typeReachable (TypeFunction ffrom fto) to | fto == to  = return . Just . fromInteger $
    depthOf ffrom

typeReachable _ _ = return Nothing

-- typeReachable to from = ggError "typeReachable"
--     ["to" <+> pretty to
--     ,"from" <+> pretty from
--     ]


-- returns a expr that contraints the Ref
exprFromRefTo :: Var ->Type -> GG Expr
exprFromRefTo var@(Var ref tyFrom) tyTo = do
    addLog "exprFromRefTo" []
    -- TO DO inefficient
    -- minSteps <- typeReachable tyTo tyFrom

    d <- gets depth_
    addLog "exprFromRefTo" ["depth, ref, fromTy, tyTo"
                           ,pretty d,  pretty ref, pretty tyFrom, pretty tyTo  ]

    process (EVar var) tyFrom


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
   Type -> -- current's Type
   Type -> -- Final Destination type
    --TO DO should be GG [ GG (Expr, Type) ] ?
    GG [ (Expr,Type) ] -- A list of possible transformations + their type

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
    ff ::Type ->Type -> GG [(Expr,Type)]
    ff TypeBool TypeInt  = do
        return [ ( opToInt cur , TypeInt)  ]

    ff (TypeSet _) TypeInt = do
        return [( opBar cur , TypeInt) ]

    ff ty TypeBool = do
        op <- boolOpFor ty
        return $ [(op cur cur, TypeBool)]

    ff from to@(TypeSet inner) | from == inner  = do
        return [(ELit $ AbsLitSet $ [cur], to )]

    ff (TypeTuple inners) to | any (==to) inners = do
        let withIdx = zip inners [1..]
            possible =  filter (\(f,_) -> f == to ) withIdx

        (_, cIndex) <- elements2 possible
        return $ [  (opIndex cur (ECon $ ConstantInt cIndex)  , to)  ]

    ff (TypeFunction ffrom fto) to | fto == to = do
        indexer <-  exprOf ffrom
        return [ (opApply cur indexer , to) ]

    ff (TypeMatrix _ inner) to | inner == to = do
        addLog "ffmat" []
        indexer <- exprOf TypeInt
        return $ [ (opIndex cur (indexer ), to) ]


    ff from to = ggError "ff unmatched" $ [  "cur" <+> pretty cur
                                          , "tyFrom" <+> pretty from
                                          , "tyTo" <+> pretty to ]
