{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen.Arbitrary.Arbitrary where

import Gen.Prelude
import Gen.Arbitrary.Expr

import qualified Data.Map as M
import qualified Data.Text as T


spec :: Depth -> Generators -> Gen (Spec, LogsTree)
spec  depth _ | depth < 0 = error "spec'' depth < 0"
spec  depth gens  = do
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
