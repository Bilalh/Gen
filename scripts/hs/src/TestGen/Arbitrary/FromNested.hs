{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, ScopedTypeVariables#-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE LambdaCase, MultiWayIf, TemplateHaskell #-}

module TestGen.Arbitrary.FromNested where

import TestGen.Arbitrary.Helpers.Prelude
import TestGen.Arbitrary.Expr
import TestGen.Arbitrary.Type

import qualified Data.Map as M
import Data.Maybe(isJust)


-- returns a Ref to var that be reached with the remaining depth
{-
    e.g. if want  Tint and we have a var
        find tu :  tuple( int, int )

    this could return depending on the depth
        tu[0]        # depth 2   (indexing counts as lavel)
        tu[ |{0}| ]  # depth 4

    e.g. TBool  from
        find f :  set of int -> bool

    this could return depending on the depth
        f( {1} )   # depth 4

-}
nestedVarsOf :: Type -> GG (Maybe (GG Expr))
nestedVarsOf ty = gets beConstant_ >>= \case
    True  -> return Nothing
    False -> nestedVarsOf' ty  -- TODO too strict?

nestedVarsOf' :: Type -> GG (Maybe (GG Expr))
nestedVarsOf' tyTo = do
    addLog "nestedVarsOf" []

    -- FIXME combine with newVars
    -- FIXME are types enough
    doms     <- gets doms_
    quanVars <- gets newVars_
    let refs =  quanVars  ++ (toTy <$> M.toList doms)

    gets depth_ >>= \d -> case (d, refs) of
        (0, _) -> return Nothing
        (_, []) -> return Nothing
        (_, _) ->  do
            filterM ( \(_, from) -> isJust <$> typeReachable tyTo from) refs >>=
                 \case
                    [] -> return Nothing
                    xs -> do
                        tu@(name,tyFrom) <- elements2 xs
                        return . Just $  exprFromRefTo tu tyTo


    where
        toTy (ref, fg) = (ref, typeOfDom . domOfFG $ fg )

-- Just steps  if a type can be reached within specifed depth
typeReachable :: Type -> Type -> GG (Maybe Int)
typeReachable to from = $notImplemented


exprFromRefTo :: (Ref,Type) -> Type -> GG Expr
exprFromRefTo (ref,fromTy) tyTo = $notImplemented
