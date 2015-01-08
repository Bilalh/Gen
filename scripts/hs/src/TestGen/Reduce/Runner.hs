{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module TestGen.Reduce.Runner where

import TestGen.Prelude
import TestGen.Reduce.Data

import TestGen.Helpers.Runner


import qualified Data.Map as M

import System.FilePath((</>))
import System.Directory(createDirectoryIfMissing)

import qualified Data.Text as T
import qualified Data.Text.IO as T


-- reads a .specE file
readSpecE :: FilePath -> IO SpecE
readSpecE fp = do
    con <- T.readFile fp
    let st = T.unpack $ con
    return . read $ st


-- True means rrError still happens
runSpec :: SpecE -> RR Bool
-- runSpec spE = chooseR (False, True)
runSpec spE = do

    containHashAdd spE >>= \case
      True -> return False
      False -> do
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


        rrErrorKind   <- gets oErrKind_
        rrErrorStatus <- gets oErrStatus_

        let
            samerrError (Left SettingI{successful_=False, data_=RefineM ms})
                | modelRefinerrError rrErrorKind =

                let statuses = M.toList $  M.map (status_) ms
                in any (\(name,status) -> status == rrErrorStatus) statuses


            samerrError (Right (_, SettingI{successful_=False,data_=SolveM ms })) =
                let
                    f ResultI{last_status, erroed= Just index, results } =
                        let ix = results !! index
                        in (kind_ ix, status_ ix)

                    kss = M.toList $  M.map f ms
                in any (\(name,ks) -> ks == (rrErrorKind,rrErrorStatus) ) kss

            samerrError _ = False


        let stillErroed  = samerrError res

        liftIO $ print $ ("HasrrError?", stillErroed)
        liftIO $ putStrLn "\n\n"
        return stillErroed



modelRefinerrError :: KindI -> Bool
modelRefinerrError RefineCompact_ = True
modelRefinerrError RefineRandom_  = True
modelRefinerrError RefineAll_     = True
modelRefinerrError _              = False
