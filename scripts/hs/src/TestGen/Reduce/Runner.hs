{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TestGen.Reduce.Runner where

import TestGen.Prelude
import TestGen.Reduce.Data

import TestGen.Helpers.Runner


import qualified Data.Map as M

import System.FilePath((</>))
import System.Directory(createDirectoryIfMissing)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Arrow((&&&))


-- reads a .specE file
readSpecE :: FilePath -> IO SpecE
readSpecE fp = do
    con <- T.readFile fp
    let st = T.unpack $ con
    return . read $ st


-- Just means rrError still happens
runSpec :: SpecE -> RR (Maybe RunResult)
runSpec spE = do

    containHashAdd spE >>= \case
      True -> return Nothing
      False -> do
        let sp = toSpec spE
        outdir <- gets outputDir_

        ts <- liftIO $ timestamp >>= return . show
        -- My laptop is too fast
        ts_num <- chooseR (1000 :: Int, 9999) >>= return . show

        let path = outdir </> (ts ++ "_" ++ ts_num)
        liftIO $ createDirectoryIfMissing True  path
        liftIO $ writeFile (path </> "spec.specE" ) (show spE)

        -- removeDirectoryRecursive breaks if dir eixsts
        -- liftIO $ createDirectoryIfMissing True path >> removeDirectoryRecursive path

        seed <- chooseR (0, 2^(24 :: Int))
        -- TODO follow logs
        perSpec <- gets specTime_
        res <- liftIO $  runToolchain' seed 4 sp path perSpec True True


        rrErrorKind   <- gets oErrKind_
        rrErrorStatus <- gets oErrStatus_

        addLog "runSpec" [nn "rrK" rrErrorKind
                         ,nn "rrS" rrErrorStatus
                         ,nn "res" (pretty . groom $ res)]

        let
            sameError :: Either RefineR (RefineR, SolveR) -> (Bool, Maybe RunResult)
            sameError (Left SettingI{successful_=False, data_=RefineM ms})
                | modelRefineError rrErrorKind =

                let sks = M.toList $  M.map ( status_ &&& kind_) ms
                in case anyFirst (rrErrorStatus,rrErrorKind) sks of
                  Just (resErrStatus_,resErrKind_) ->
                      (True, Just $ RunResult{resDirectory_ = path
                                              ,resErrKind_
                                              ,resErrStatus_})

                  Nothing -> (False, Just $ RunResult{resDirectory_ = path
                                                   ,resErrKind_   = fstKind sks
                                                   ,resErrStatus_ = fstStatus sks})

                where
                  anyFirst (StatusAny_,KindAny_) ((_,(x,y)):_) = Just (x,y)
                  anyFirst (StatusAny_, ki)      ((_,(x,_)):_) = Just (x,ki)
                  anyFirst (si,KindAny_)       v@((_,(_,y)):_) =
                      if any (\(_,(s,_)) -> s == si ) v then
                          Just (si,y)
                      else
                          Nothing

                  anyFirst skIn sks =
                      if any (\(_,sk) -> sk == skIn ) sks then
                          Just skIn
                      else
                          Nothing


            sameError (Right (_, SettingI{successful_=False,data_=SolveM ms })) =
                let
                    f ResultI{erroed= Just index, results } =
                        let ix = results !! index
                        in Just (status_ ix, kind_ ix)
                    f _ = Nothing

                    sks = M.toList $  M.mapMaybe f ms

                in case anyFirst (rrErrorStatus,rrErrorKind) sks of
                   Just (resErrStatus_,resErrKind_)   ->
                       (True, Just $ RunResult{resDirectory_ = path
                                              ,resErrKind_
                                              ,resErrStatus_})
                   Nothing -> (False, Just $ RunResult{resDirectory_ = path
                                                      ,resErrKind_   = fstKind sks
                                                      ,resErrStatus_ = fstStatus sks})

                where
                  anyFirst (StatusAny_,KindAny_) ((_,(x,y)):_) = Just (x,y)
                  anyFirst (StatusAny_,ki) v@((_,(x,_)):_) =
                      if any (\(_,(_,k)) -> k == ki ) v then
                          Just (x,ki)
                      else
                          Nothing

                  anyFirst (si,KindAny_) v@((_,(_,y)):_) =
                      if any (\(_,(s,_)) -> s == si ) v then
                          Just (si,y)
                      else
                          Nothing

                  anyFirst skIn sks =
                      if any (\(_,sk) -> sk == skIn ) sks then
                          Just skIn
                      else
                          Nothing



            sameError _ = (False, Nothing)

            fstStatus []            = error "fstStatus no statuses"
            fstStatus ((_,(s,_)):_) = s

            fstKind []            = error "fstKind no kinds"
            fstKind ((_,(_,k)):_) = k



        let stillErroed  = sameError res

        liftIO $ print $ ("HasrrError?" :: String, fst stillErroed)
        liftIO $ putStrLn "\n\n"
        case stillErroed of
          (True, Just r)   -> return $ Just $ r
          (True, Nothing)  -> rrError "Same error but no result" []
          (False, Just r)  -> do
            addOtherError r
            return Nothing

          (False, Nothing) -> return Nothing



modelRefineError :: KindI -> Bool
modelRefineError RefineCompact_ = True
modelRefineError RefineRandom_  = True
modelRefineError RefineAll_     = True
modelRefineError KindAny_       = True
modelRefineError _              = False


addOtherError :: RunResult -> RR ()
addOtherError r = do
  return ()
  modify $ \st -> st{otherErrors_ =r : otherErrors_ st }
