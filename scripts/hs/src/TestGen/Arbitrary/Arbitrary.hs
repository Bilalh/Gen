{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Arbitrary where

import TestGen.Prelude
import TestGen.Arbitrary.Data
import TestGen.Arbitrary.Domain
import TestGen.Arbitrary.Expr

import qualified Data.Map as M
import qualified Data.Text as T
import Data.List

data WithLogs a = WithLogs a LogsTree

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
    arbitrary = needed undefined
        
        where 
        needed :: ArbSpec a => a -> Gen (WithLogs a)
        needed unused = fmap (\(sp,t) -> WithLogs (wrapSpec sp) t ) 
                     <$> sized $ (flip  spec'') (tyGens unused)


instance Arbitrary SpecE where
    arbitrary = sized spec
    shrink (SpecE ds es) =
        let sps = map (SpecE ds) (tails2 es)
        in sps

instance ArbSpec SpecE where 
    tyGens   = def
    getSpec  = id
    wrapSpec = id
    

tails2 :: [a] -> [[a]]
tails2 [] = []
tails2 xs = tail (tails xs)


spec :: Depth -> Gen SpecE
spec depth =  do
    (specE, _) <- spec' depth
    return specE

spec' :: Depth -> Gen (SpecE, LogsTree) 
spec' = flip spec'' def


spec'' :: Depth -> Generators -> Gen (SpecE, LogsTree) 
spec'' depth _ | depth < 0 = error "spec'' depth < 0"
spec'' depth gens  = do
    let state = def{depth_= depth `div` 2, generators_=gens}
    let domsCount = (1, min ((depth+1)*2) 10)
    let exprCount = (0, min ((depth+1)*2) 10)
    
    (doms,state') <- runStateT  ( listOfBounds domsCount dom) state


    let withNames =  zipWith (\d i -> (name i , Find d)) doms [1 :: Int ..]
    let mappings  = M.fromList withNames


    let state'' =  state'{doms_=mappings, depth_ =depth, nextNum_ = length doms + 1}
    (exprs,sfinal) <- runStateT (listOfBounds exprCount expr) state''

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
        return $ (SpecE mappings exprs, logs_ sfinal)

    where name i =  T.pack $  "var" ++  (show  i)


    
