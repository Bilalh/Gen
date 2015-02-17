{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Arbitrary where

import TestGen.Prelude
-- import TestGen.Arbitrary.Data
-- import TestGen.Arbitrary.Domain
import TestGen.Arbitrary.Expr

import qualified Data.Map as M
import qualified Data.Text as T
import Data.List

import qualified Test.QuickCheck as QC

data WithLogs a = WithLogs a LogsTree
data WithExtra a = WithExtra {
        inner_ :: a
        , wlogs_  :: LogsTree
        , ts_int_ :: Int
        , run_seed_ :: Int
        }


arbitraryDef :: ArbSpec a => a -> Gen a
arbitraryDef unused = fmap (wrapSpec . fst) <$> sized $ (flip  spec'') (tyGens unused)


instance (ArbSpec a, Show a, Arbitrary a) => ArbSpec (WithLogs a) where
    getSpec (WithLogs aa _) = getSpec aa
    tyGens  aa              = tyGens aa
    wrapSpec                = error "wrapSpec, would not have logs"

instance (Show a) => Show (WithLogs a) where
    show (WithLogs inner _) =
        "WithLogs( " ++ show inner ++ " )"

instance (Arbitrary a, ArbSpec a) => Arbitrary (WithLogs a) where
    arbitrary = needed (error "Arbitrary (WithLogs a)")

        where
        needed :: ArbSpec a => a -> Gen (WithLogs a)
        needed unused = fmap (\(sp,t) -> WithLogs (wrapSpec sp) t )
                     <$> sized $ (flip  spec'') (tyGens unused)


instance (ArbSpec a, Show a, Arbitrary a) => ArbSpec (WithExtra a) where
    getSpec WithExtra{..}    = getSpec inner_
    tyGens  aa              = tyGens aa
    wrapSpec                = error "wrapSpec, would not have extra state"

instance (Show a) => Show (WithExtra a) where
    show WithExtra{..} =
        "WithExtra( " ++ show inner_ ++ " )"

instance (Arbitrary a, ArbSpec a) => Arbitrary (WithExtra a) where
    arbitrary = needed (error "Arbitrary (WithExtra a)")

        where
        needed :: ArbSpec a => a -> Gen (WithExtra a)
        needed unused = sized $ genn (tyGens unused)

        genn gens size = do
            (sp,t)    <- spec'' size gens
            ts_int_   <- QC.choose (10,99)
            run_seed_ <- QC.choose (0,2^(24:: Integer ))

            return WithExtra{inner_=wrapSpec sp, wlogs_= t, ts_int_, run_seed_ }


instance Arbitrary Spec where
    arbitrary = sized spec

instance ArbSpec Spec where
    tyGens   = def
    getSpec  = id
    wrapSpec = id


tails2 :: [a] -> [[a]]
tails2 [] = []
tails2 xs = tail (tails xs)


spec :: Depth -> Gen Spec
spec depth =  do
    (specE, _) <- spec' depth
    return specE

spec' :: Depth -> Gen (Spec, LogsTree)
spec' = flip spec'' def


specwithLogs :: Depth -> Generators -> Gen (WithLogs Spec)
specwithLogs depth gens  =  uncurry WithLogs  <$> spec'' depth gens

spec'' :: Depth -> Generators -> Gen (Spec, LogsTree)
spec'' depth _ | depth < 0 = error "spec'' depth < 0"
spec'' depth gens  = do
    let state =  def{depth_= (depth+1) `div` 2, generators_=gens}


    let domsCount = (1, min ((depth+1)*2) 10)
    let exprCount = (0, min ((depth+1)*2) 10)

    (doms,state') <- runStateT (addDepth "domDepth" >> listOfBounds domsCount dom) state


    let withNames =  zipWith (\d i -> (name i , Findd d)) doms [1 :: Int ..]
    let mappings  = M.fromList withNames


    let state'' =  state'{doms_=mappings, depth_ =depth + 1, nextNum_ = length doms + 1}
    (exprs,sfinal) <- runStateT
            (  addDepth "exprDepth"
            >> listOfBounds exprCount expr
            >>= addStateLog
            )
        state''

    if length withNames == 0 then
        error . show $  vcat . map pretty  $
            [ "~~------~~"
            , "both doms  zero length"
            , nn "depth" depth
            , nn "domsCount" domsCount
            , nn "exprCount" exprCount
            , nn "depthlen" (length doms)
            , nn "doms" (vcat . map pretty $ doms)
            , nn "withNames" (vcat . map pretty $ withNames)
            , nn "exprs" (vcat . map pretty $ exprs)
            , nn "state" state
            , nn "state'" state'
            , nn "state''" state''
            , nn "sfinal" sfinal
            , nn "state" (show state)
            , nn "state'" (show state')
            , nn "state''" (show state'')
            , nn "sfinal" (show sfinal)
            , "|------|"
            ]
    else
        return $ (Spec mappings exprs Nothing, logs_ sfinal)

    where
        name i =  T.pack $  "var" ++  (show  i)
        addDepth s= do
            d <- gets depth_
            addLog s [pretty d]
        --
        addStateLog es = do
            ss <- get
            addLog "finalState" [pretty ss]
            return es
