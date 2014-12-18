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


-- True means error still happens
runSpec :: SpecE -> RR Bool
runSpec spE = do
    let sp = toSpec spE
    outdir <- gets outputdir_

    ts <- liftIO $ timestamp >>= return . show
    ts_num <- rndRangeM (100 :: Int, 999) >>= return . show
    
    let path = outdir </> (ts ++ "_" ++ ts_num)
    
    -- removeDirectoryRecursive breaks if dir eixsts
    -- liftIO $ createDirectoryIfMissing True path >> removeDirectoryRecursive path
    
    seed <- rndRangeM (0, 2^24)
    -- TODO follow logs
    res <- liftIO $  runToolchain' seed 4 sp path 20 True True
    

    errorKind <- gets oErrKind_
    let 
        sameError (Left SettingI{successful_=False}) | modelRefineError errorKind = True 
        sameError (Right (_, SettingI{successful_=False,data_=SolveM ms })) = 
        
            let
                f ResultI{last_status, erroed= Just index, results } = 
                    let kind = kind_ (results !! index)
                    in kind
        
                kinds = M.toList $  M.map f ms
            in any (\(name,kind) -> kind == errorKind) kinds
 
        sameError _ = False
    
    
    let stillErroed  = sameError res
     
    liftIO $ print $ (stillErroed, pretty sp)
    return stillErroed



modelRefineError :: KindI -> Bool
modelRefineError RefineCompact_ = True
modelRefineError RefineRandom_  = True
modelRefineError RefineAll_     = True
modelRefineError _              = False