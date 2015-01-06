{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestGen.Reduce.Runner where

import TestGen.Prelude
import TestGen.Reduce.Data

import TestGen.Helpers.Runner

import Common.Helpers(timestamp)

import qualified Data.Map as M

import System.FilePath((</>), (<.>), takeFileName)
import System.Directory(createDirectoryIfMissing, removeDirectoryRecursive)

import qualified Data.Text as T
import qualified Data.Text.IO as T

-- reads a .specE file
readSpecE :: FilePath -> IO SpecE
readSpecE fp = do
    con <- T.readFile fp
    let st = T.unpack $ con
    return . read $ st 
    

-- True means error still happens
runSpec :: SpecE -> RR Bool
-- runSpec spE = chooseR (False, True)
runSpec spE = do
    let sp = toSpec spE
    outdir <- gets outputdir_

    ts <- liftIO $ timestamp >>= return . show
    ts_num <- chooseR (100 :: Int, 999) >>= return . show
    
    let path = outdir </> (ts ++ "_" ++ ts_num)
    liftIO $ createDirectoryIfMissing True  path
    liftIO $ writeFile (path </> "spec.specE" ) (show spE)
    
    -- removeDirectoryRecursive breaks if dir eixsts
    -- liftIO $ createDirectoryIfMissing True path >> removeDirectoryRecursive path
    
    seed <- chooseR (0, 2^24)
    -- TODO follow logs
    res <- liftIO $  runToolchain' seed 4 sp path 20 True True
    

    errorKind   <- gets oErrKind_
    errorStatus <- gets oErrStatus_
    
    let 
        sameError (Left SettingI{successful_=False, data_=RefineM ms}) 
            | modelRefineError errorKind = 
            
            let statuses = M.toList $  M.map (status_) ms
            in any (\(name,status) -> status == errorStatus) statuses
            
        
        sameError (Right (_, SettingI{successful_=False,data_=SolveM ms })) = 
            let
                f ResultI{last_status, erroed= Just index, results } = 
                    let ix = results !! index
                    in (kind_ ix, status_ ix)
        
                kss = M.toList $  M.map f ms
            in any (\(name,ks) -> ks == (errorKind,errorStatus) ) kss
 
        sameError _ = False
    
    
    let stillErroed  = sameError res
     
    liftIO $ print $ ("HasError?", stillErroed)
    liftIO $ putStrLn "\n\n"
    return stillErroed



modelRefineError :: KindI -> Bool
modelRefineError RefineCompact_ = True
modelRefineError RefineRandom_  = True
modelRefineError RefineAll_     = True
modelRefineError _              = False
