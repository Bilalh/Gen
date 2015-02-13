{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module EssenceSolver.Checker where

import EssenceSolver.Data

import Language.E

import qualified Data.Map as M

-- Returns True if any constraint is not satisfied
violates  :: [Constraint] -> Env -> Bool
violates cs env =
    let (mresult, _logs) = runCompESingle "violates" helper
    in case mresult of
        -- Right b    -> tracePretty ["violates result" <+> pretty b] b
        Right b    -> b
        Left d     -> error . show .  vcat $ ["violates", d, pretty _logs]

    where
    helper :: MonadConjure m => m Bool
    helper = do
        mapM_ (\(n,e) -> addReference n e )  env

        violated :: Bool <- or <$> mapM eViolates cs
        return violated

    eViolates :: MonadConjure m =>  Constraint -> m Bool
    eViolates e = do
        simplifed <- fullySimplifyE e
        res <- toBool simplifed
        return $ case res of
            Right (b,x) -> traceHang ("EV" <+> pretty (not b))
                [vcat (map pretty x), prettyEnv env, pretty e]
                           $ not b
            Left m -> tracePretty ["eViolates constraint" <+> pretty m, prettyEnv env] False



domSizeC :: E -> E
domSizeC e  =
    let (mresult, _logs) = runCompESingle "eguard" helper
    in case mresult of
        Right size ->  size
        Left d     -> error . show .  vcat $ ["eguard", d, pretty _logs]

    where
        helper = do
            f <- domSize e
            fullySimplifyE f


eSatisfied :: [(Ref, E)] -> E -> Bool
eSatisfied vs e =  subAndCheck

    where
    subAndCheck =
        let
            (subbedE, _logs) = runCompESingle "subVals"  (subVals vs e)
        in
            case subbedE of
                Left  x -> error . show $ vcat ["subAndCheck", x]
                Right ne ->
                    let sat =  eguard ne  in
                    trace (show $ vcat [pretty ne, pretty sat]) $  eguard ne


subVals  :: MonadConjure m => [(Ref, E)] -> E ->  m E
subVals lettings expr =
    let
        lettingsMap = M.fromList lettings

        f x | Just y <- M.lookup x lettingsMap = transform f y
        f x = x
    in
        return $ transform f expr

fullySimplifyE :: MonadConjure m => E -> m E
fullySimplifyE = liftM fst . runWriterT . fullySimplify

eguard :: E -> Bool
eguard e =
    let (mresult, _logs) = runCompESingle "eguard" helper
    in case mresult of
        Right b -> b
        Left d  -> error . show .  vcat $ ["eguard", d]

    where
        helper = do
            simplifed <- fullySimplifyE e
            res <- toBool simplifed
            return $ case res of
                Right (b,_) -> b
                Left m  -> error . show $ vcat ["eguard fail", pretty m]
