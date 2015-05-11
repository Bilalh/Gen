{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Spec where

import Gen.Essence.Domain ()
import Gen.Essence.Expr   ()
import Gen.Essence.St
import Gen.Essence.Rnd
import Gen.Imports

import qualified Data.Map as M


instance Generate Spec where
  give GNone = do
    depth <- gets depth
    let domsCount = (1, min ((depth+1)*2) 7)
    let exprCount = (0, min ((depth+1)*2) 7)
    i_d <- choose3 domsCount
    i_e <- choose3 exprCount
    -- let (i_d, i_e) = (1 :: Integer, 1 :: Integer)

    doms <- mapM (\_ -> give GNone) [1..i_d]
    let withNames =  zipWith (\d i -> (name i , Findd d)) doms [1 :: Int ..]
    let mappings  = M.fromList withNames
    modify $ \st -> st{doms_=mappings}

    exprs <- mapM (const . give $ GType TypeBool ) [0..i_e]
    return $ Spec mappings exprs Nothing

    where name i =  stringToText $  "var" ++  (show  i)

  give t = giveUnmatched "Generate (Spec)" t

  possible _ _ = return True
