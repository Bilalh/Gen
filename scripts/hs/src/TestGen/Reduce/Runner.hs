{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestGen.Reduce.Runner where

import TestGen.Prelude
import TestGen.Reduce.Data

import TestGen.Helpers.Runner

import qualified Data.Map as M

import System.Directory(createDirectoryIfMissing, removeDirectoryRecursive)


-- True means error still happens
runSpec :: SpecE -> RR Bool
runSpec spE = do
    let sp = toSpec spE
    let path = "/Users/bilalh/CS/break_conjure/fixed/46c3d2b43f4e/2014-12-10_02-01_1418176894/_errors/Validate_/ErrorUnknown_/1418178864_89/out"
    
    -- removeDirectoryRecursive breaks if dir eixsts
    liftIO $ createDirectoryIfMissing True path >> removeDirectoryRecursive path
    
    seed <- rndRangeM (0, 2^24)
    res <- liftIO $  runToolchain' seed 4 sp (path) 120 True
    

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
modelRefineError _              = False
