{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Type where

import Gen.Essence.St
import Gen.Imports
import Gen.Essence.Rnd

import qualified Data.Set as S

instance Generate Type where
  give (GOnlyTopLevel ws) = do
    logDepthCon $line (GOnlyTopLevel ws)
    defs <- gets depth >>= \d ->
     if | d < 0     -> nnError "GenerateType invaild Depth: " ["depth" <+> pretty d]
        | d == 0    -> return [ (K_TypeBool, pure TypeBool)
                              , (K_TypeInt,  pure TypeInt)
                              ]
        | otherwise -> return
            [ (K_TypeBool,      pure TypeBool)
            , (K_TypeInt,       pure TypeInt)
            , (K_TypeSet,       liftM TypeSet       (dgive GNone))
            , (K_TypeMSet,      liftM TypeMSet      (dgive GNone))
            , (K_TypePartition, liftM TypePartition (dgive GNone))
            , (K_TypeTuple,     liftM TypeTuple     (bounded3 (1,4) $ dgive GNone))
            , (K_TypeRelation,  liftM TypeRelation  (bounded3 (1,4) $ dgive GNone))

            , (K_TypeMatrix,    pure TypeMatrix   `ap` pure TypeInt
                                                  `ap` dgive GNone )
            , (K_TypeFunction,  pure TypeFunction `ap` dgive GNone
                                                  `ap` dgive GNone )
            ]

    let allowed = S.fromList ws
    let ws' = [ (k,0) | k <- fieldKeys (Proxy :: Proxy Type), k `S.notMember` allowed ]

    parts <- withWeights ws' $ getWeights defs
    freqError $line (pretty $ groom ws') parts
    frequency3 parts

  give GNone = do
    logStats $line GNone
    defs <- gets depth >>= \d ->
     if | d < 0     -> nnError "GenerateType invaild Depth: " ["depth" <+> pretty d]
        | d == 0    -> return [ (K_TypeBool, pure TypeBool)
                              , (K_TypeInt,  pure TypeInt)
                              ]
        | otherwise -> return
            [ (K_TypeBool,      pure TypeBool)
            , (K_TypeInt,       pure TypeInt)
            , (K_TypeSet,       liftM TypeSet       (dgive GNone))
            , (K_TypeMSet,      liftM TypeMSet      (dgive GNone))
            , (K_TypePartition, liftM TypePartition (dgive GNone))
            , (K_TypeTuple,     liftM TypeTuple     (bounded3 (1,4) $ dgive GNone))
            , (K_TypeRelation,  liftM TypeRelation  (bounded3 (1,4) $ dgive GNone))

            , (K_TypeMatrix,    pure TypeMatrix   `ap` pure TypeInt
                                                  `ap` dgive GNone )
            , (K_TypeFunction,  pure TypeFunction `ap` dgive GNone
                                                  `ap` dgive GNone )
            ]



    parts <- getWeights defs
    picked <- frequency3 parts
    logInfo2 $line [nn "ret" picked]
    return picked


  give t = giveUnmatched "Generate (Type)" t

  possible _ _  = return True
