{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module TestGen.Essence.Generate(generate) where

import Conjure.Language.Definition
import TestGen.Prelude
import TestGen.Helpers.IO
import TestGen.Helpers.Runner
import TestGen.Arbitrary.Arbitrary

generateEssence :: EssenceConfig -> IO ()
generateEssence EssenceConfig{..}= do
  forM_ [1..1000] $  \_ -> do
    (aSpec,_)  <- generate $ spec'' size_ def{gen_useFunc = myUseFunc}
    putStrLn . show . pretty $ aSpec


_generateDebug :: IO ()
_generateDebug = do
    let ec = def
             { outputDirectory = "aa"
             , mode_           = Refine_

             , totalTime   = 20
             , perSpecTime = 5
             , size_       = 2
             , cores_      = 1
             , seed_       = 44

             , binariesDirectory = Nothing
             , oldConjure        = False
             }
    generateEssence ec

-- Does not work completely
myUseFunc Aapply = False
myUseFunc Ahist = False
myUseFunc Ainverse = False
myUseFunc _ = True
