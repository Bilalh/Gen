{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Type where

import Gen.Essence.St
import Gen.Imports
import Gen.Essence.Rnd
import qualified Data.Set as S

instance Generate Type where

  give GOnlyLiteralTypes = give $ GOnlyTopLevel
    [K_TypeSet, K_TypeMSet, K_TypeFunction, K_TypeRelation]

  give (GOnlyTopLevel ws) = do
    defs <- gets depth >>= \d ->
     if | d < 0     -> error $ "GenerateType invaild Depth: " ++ show d
        | d == 0    -> return [ ("TypeBool", pure TypeBool)
                              , ("TypeInt",  pure TypeInt)
                              ]
        | otherwise -> return [
                         ("TypeBool", pure TypeBool)
                       , ("TypeInt",  pure TypeInt)
                       , ("TypeSet",   liftM TypeSet   (withDepthDec (give GNone) ))
                       -- , ("TypeMatrix", liftM TypeMatrix (withDepthDec (give GNone) ))
                       -- , ("TypeMSet",  liftM TypeMSet  (withDepthDec (give GNone) ))
                       -- , ("TypePartition",   liftM TypePartition   (withDepthDec (give GNone) ))
                       ]

    let allowed = S.fromList ws
    let ws' = [ (k,0) | k <- fieldKeys (Proxy :: Proxy Type), k `S.notMember` allowed ]

    parts <- withWeights ws' $ getWeights defs
    frequency3 parts

  give GNone = do
    defs <- gets depth >>= \d ->
     if | d < 0     -> error $ "GenerateType invaild Depth: " ++ show d
        | d == 0    -> return [ ("TypeBool", pure TypeBool)
                              , ("TypeInt",  pure TypeInt)
                              ]
        | otherwise -> return [
                         ("TypeBool", pure TypeBool)
                       , ("TypeInt",  pure TypeInt)
                       , ("TypeSet",   liftM TypeSet   (withDepthDec (give GNone) ))
                       -- , ("TypeMatrix", liftM TypeMatrix (withDepthDec (give GNone) ))
                       -- , ("TypeMSet",  liftM TypeMSet  (withDepthDec (give GNone) ))
                       -- , ("TypePartition",   liftM TypePartition   (withDepthDec (give GNone) ))
                       ]

    parts <- getWeights defs
    frequency3 parts


  give t = giveUnmatched "Generate (Type)" t

  possible _ _  = return True
