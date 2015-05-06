{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Spec where

import Gen.Essence.Constant           ()
import Gen.Essence.Domain             ()
import Gen.Essence.Expr               ()
import Gen.Essence.Literal            ()
import Gen.Essence.Op                 ()
import Gen.Essence.Range              ()
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.StandardImports

import qualified Data.Map as M


instance Generate Spec where
  give GNone = do
    depth <- gets depth
    let domsCount = (1, min ((depth+1)*2) 7)
    let exprCount = (0, min ((depth+1)*2) 7)
    i_d <- choose3 domsCount
    i_e <- choose3 exprCount

    doms <- mapM (\_ -> give GNone) [1..i_d]
    let withNames =  zipWith (\d i -> (name i , Findd d)) doms [1 :: Int ..]
    let mappings  = M.fromList withNames
    modify $ \st -> st{doms_=mappings}

    exprs <- mapM (\_ -> give (GType TypeBool) ) [0..i_e]
    return $ Spec mappings exprs Nothing

    where name i =  stringToText $  "var" ++  (show  i)

  give t = giveUnmatched "Generate (Spec)" t

  possiblePure _ _ _ = True
