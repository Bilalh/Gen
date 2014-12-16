{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestGen.Reduce.Runner where

import TestGen.Prelude
import TestGen.Helpers.Runner

import qualified Data.Map as M

import System.Directory(createDirectoryIfMissing, removeDirectoryRecursive)


-- True means error still happens
runSpec :: SpecE -> IO Bool
runSpec spE = do
    let sp = toSpec spE
    let path = "/Users/bilalh/CS/break_conjure/fixed/46c3d2b43f4e/2014-12-10_02-01_1418176894/_errors/Validate_/ErrorUnknown_/1418178864_89/out"
    
    -- removeDirectoryRecursive breaks if dir eixsts
    createDirectoryIfMissing True path >> removeDirectoryRecursive path
    
    res <- runToolchain' 33 4 sp (path) 120 True
    let stillErroed  = sameError res
     
    print $ (stillErroed, pretty sp)
    return stillErroed

    where 
    -- This would be a different error
    sameError (Left SettingI{successful_=False}) = False
    sameError (Right (_, SettingI{successful_=False,data_=SolveM ms })) = 
        
        let
            f ResultI{last_status, erroed= Just index, results } = 
                let kind = kind_ (results !! index)
                in kind
        
            kinds = M.toList $  M.map f ms
        in any (\(name,kind) -> kind == Validate_) kinds
 
    sameError _ = False
