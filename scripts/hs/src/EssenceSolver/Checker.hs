{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module EssenceSolver.Checker where

-- import Language.E
import Language.E hiding (trace)
import Debug.Trace(trace)

import qualified Data.Map as M

type Ref = E

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
