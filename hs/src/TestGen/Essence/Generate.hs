{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module TestGen.Essence.Generate(generate) where

import Conjure.Language.Definition
import TestGen.Prelude
import TestGen.Helpers.IO
import TestGen.Helpers.Runner
import TestGen.Arbitrary.Arbitrary

generateEssence :: EssenceConfig -> IO ()
generateEssence EssenceConfig{..}= do
  (aSpec,_)  <- generate $ spec'' size_ def
  putStrLn . show . pretty $ aSpec


_generateDebug :: IO ()
_generateDebug = do
    let ec = def
             { outputDirectory = "aa"
             , mode_           = Refine_

             , totalTime   = 20
             , perSpecTime = 5
             , size_       = 3
             , cores_      = 1
             , seed_       = 44

             , binariesDirectory = Nothing
             , oldConjure        = False
             }
    generateEssence ec
