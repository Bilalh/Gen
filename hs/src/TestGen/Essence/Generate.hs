{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module TestGen.Essence.Generate(generateEssence) where

import Conjure.Language.Definition
import TestGen.Prelude
import TestGen.Helpers.IO
import TestGen.Helpers.Runner
import TestGen.Arbitrary.Arbitrary

import Conjure.UI.TypeCheck ( typeCheckModel )
import Conjure.UI.IO(writeModel)

generateEssence :: EssenceConfig -> IO ()
generateEssence co@EssenceConfig{..} = do


  case mode_ of
    TypeCheck_ -> runTypeCheck co

  -- forM_ [1..1000] $  \_ -> do
  --   (aSpec,_)  <- generate $ spec'' size_ def{gen_useFunc = myUseFunc}
  --   putStrLn . show . pretty $ aSpec


runTypeCheck :: EssenceConfig -> IO ()
runTypeCheck ec@EssenceConfig{..}= do
  process

  where
    process = do
      (sp,_) <- generate $ spec'' size_ def{gen_useFunc = myUseFunc}
      model :: Model <- toConjure sp


      let (res :: Either Doc ())  =ignoreLogs $ typeCheckModel model
      handleResult sp model res
      process


    handleResult sp model (Left d) = do
      putStrLn . show . pretty $ model
      putStrLn . show $ d
      writeFailing ec sp model d

    handleResult _ _ (Right _) = do
      return ()


writeFailing :: EssenceConfig -> Spec -> Model -> Doc -> IO ()
writeFailing EssenceConfig{..} sp model errDoc = do
  ts <- timestamp >>= return . show
  let dir = outputDirectory_ </> "_typecheck" </> ts

  createDirectoryIfMissing True dir
  writeToJSON (dir </> "spec.spec.json") sp
  writeModel (Just (dir </> "spec.essence") ) model

  writeFile (dir </> "spec.error") $ show . vcat $
                [ errDoc
                , "----"
                , pretty model
                ]


-- Does not work completely
myUseFunc Aapply = False
myUseFunc Ahist = False
myUseFunc Ainverse = False
myUseFunc _ = True
