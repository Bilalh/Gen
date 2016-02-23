{-# LANGUAGE Rank2Types #-}
module Gen.Reduce.Point where

import Gen.Imports
import Gen.Instance.Point
import Gen.Instance.UI
import Gen.IO.Formats
import Gen.IO.Toolchain   (writeModelDef)
import Gen.Reduce.Data
import Gen.Reduce.Random

import qualified Control.Exception as Exc


-- | Generates a new point for a spec if possible
generatePoint :: Spec -> RRR (Maybe Point)
generatePoint spec = do
  RState{rconfig=RConfig{outputDir_}} <- get
  ts  <- timestamp >>= return . show
  num :: Int <- chooseR (10 :: Int ,99)
  let uname  =  (ts ++ "_" ++ (show num) )
  let tmp = outputDir_ </> "_paramGen" </> uname

  case toConjure spec of
    Nothing      -> return Nothing
    (Just model) -> do
      essencePath <- writeModelDef tmp model
      seed :: Int <- chooseR (0 :: Int , 2^(24 :: Int))
      b <- liftIO $  Exc.catch (
          instances_no_racing essencePath 1 10 tmp Nothing seed LogNone >> return True)
            (\ (_ :: Exc.SomeException) -> return False )
      if not b then
        return Nothing
      else do
        -- FIXME could just the list the dir
        liftIO $ getAllFilesWithSuffix ".param" (tmp </> "_params") >>= \case
           [x] -> do
             p <- readPoint x
             return $ Just p
           _   -> do
             return Nothing
