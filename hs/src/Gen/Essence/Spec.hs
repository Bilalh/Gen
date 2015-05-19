{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Spec where

import Gen.Essence.Domain ()
import Gen.Essence.Expr   ()
import Gen.Essence.St
import Gen.Essence.Rnd
import Gen.Imports
import Gen.Essence.Objective ()

import qualified Data.Map as M


instance Generate Spec where
  give GNone = do
    depth <- gets depth
    -- TODO add to ui
    dom_depth <- choose3 (0, min depth 3)

    let domsCount = (1, min ((depth+1)*2) 7)
    let exprCount = (0, min ((depth+1)*2) 7)
    i_d <- choose3 domsCount
    i_e <- choose3 exprCount
    -- let (i_d, i_e) = (1 :: Integer, 1 :: Integer)

    logInfo2 $line [ nn "i_d" i_d
                   , nn "i_e" i_e
                   ]

    doms <- mapM (\_ -> withDepth dom_depth $ give GNone) [1..i_d]
    let withNames =  zipWith (\d i -> (name i , Findd d)) doms [1 :: Int ..]
    let mappings  = M.fromList withNames
    modify $ \st -> st{doms_=mappings}

    exprs <- mapM (const . give $ GType TypeBool ) [0..i_e]
    Spec mappings exprs <$> (give GNone)

    where name i =  stringToText $  "var" ++  (show  i)

  give t = giveUnmatched "Generate (Spec)" t

  possiblePure _ _ _ = True
  requires _ _       = [K_TypeSet]
