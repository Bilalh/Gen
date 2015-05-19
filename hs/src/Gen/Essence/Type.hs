{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Type where

import Gen.Essence.St
import Gen.Imports
import Gen.Essence.Rnd
import qualified Data.Set as S

instance Generate Type where

  give GOnlyLiteralTypes = give $ GOnlyTopLevel
    [K_TypeSet, K_TypeMSet, K_TypeTuple, K_TypeMatrix, K_TypeFunction]

  give (GOnlyTopLevel ws) = do
    defs <- gets depth >>= \d ->
     if | d < 0     -> error $ "GenerateType invaild Depth: " ++ show d
        | d == 0    -> return [ (K_TypeBool, pure TypeBool)
                              , (K_TypeInt,  pure TypeInt)
                              ]
        | otherwise -> return
            [ (K_TypeBool,      pure TypeBool)
            , (K_TypeInt,       pure TypeInt)
            , (K_TypeSet,       liftM TypeSet   (withDepthDec (give GNone) ))
            , (K_TypeMSet,      liftM TypeMSet  (withDepthDec (give GNone) ))
            , (K_TypePartition, liftM TypePartition (withDepthDec (give GNone) ))
            , (K_TypeTuple,     liftM TypeTuple (vectorOf3 3 $ withDepthDec (give GNone)) )
            , (K_TypeRelation,  liftM TypeRelation (vectorOf3 3 $ withDepthDec (give GNone)) )
            , (K_TypeMatrix,    pure TypeMatrix   `ap` pure TypeInt
                                                  `ap` withDepthDec (give GNone) )
            , (K_TypeFunction,  pure TypeFunction `ap` withDepthDec (give GNone)
                                                  `ap` withDepthDec (give GNone) )
            ]

    let allowed = S.fromList ws
    let ws' = [ (k,0) | k <- fieldKeys (Proxy :: Proxy Type), k `S.notMember` allowed ]

    parts <- withWeights ws' $ getWeights defs
    frequency3 parts

  give GNone = do
    defs <- gets depth >>= \d ->
     if | d < 0     -> error $ "GenerateType invaild Depth: " ++ show d
        | d == 0    -> return [ (K_TypeBool, pure TypeBool)
                              , (K_TypeInt,  pure TypeInt)
                              ]
        | otherwise -> return
            [ (K_TypeBool,     pure TypeBool)
            , (K_TypeInt,      pure TypeInt)
            , (K_TypeSet,      liftM TypeSet   (withDepthDec (give GNone) ))
            , (K_TypeMSet,     liftM TypeMSet  (withDepthDec (give GNone) ))
            , (K_TypeTuple,    liftM TypeTuple (vectorOf3 3 $ withDepthDec (give GNone)) )
            , (K_TypeRelation, liftM TypeRelation (vectorOf3 3 $ withDepthDec (give GNone)) )
            , (K_TypeMatrix,   pure TypeMatrix   `ap` pure TypeInt
                                                 `ap` withDepthDec (give GNone) )
            , (K_TypeFunction, pure TypeFunction `ap` withDepthDec (give GNone)
                                                 `ap` withDepthDec (give GNone) )
            ]

    parts <- getWeights defs
    frequency3 parts


  give t = giveUnmatched "Generate (Type)" t

  possible _ _  = return True
