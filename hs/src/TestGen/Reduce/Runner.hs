{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module TestGen.Reduce.Runner where

import TestGen.Classify.Meta(mkMeta)
import TestGen.Helpers.Runner
import TestGen.Prelude
import TestGen.Reduce.Data


import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T


-- reads a .specE file
readSpecE :: FilePath -> IO SpecE
readSpecE fp = do
    con <- T.readFile fp
    let st = T.unpack $ con
    return . readNote "readSpecE" $ st


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

        let meta = mkMeta spE
        liftIO $  writeFile (path </> "spec.meta" ) (show meta)
        liftIO $  writeJSON  (path </> "spec.meta.json" ) (meta)

        seed <- chooseR (0, 2^(24 :: Int))
        -- TODO follow logs
        perSpec <- gets specTime_
        res <- liftIO $  runToolchain' seed 4 sp path perSpec True True


        rrErrorKind   <- gets oErrKind_
        rrErrorStatus <- gets oErrStatus_

        addLog "runSpec" [pretty spE]
        addLog "runSpec_results" [nn "org_kind"   rrErrorKind
                                 ,nn "org_status" rrErrorStatus
                                 ,nn "res" (pretty . groom $ res)]

        let
            sameError :: Either RefineR (RefineR,SolveR) -> (Bool,Maybe RunResult)
            sameError (Left SettingI{successful_=False, data_=RefineM ms})
                | modelRefineError rrErrorKind =

                let sks = M.toList $  M.map ( status_ &&& kind_) ms
                in case anyFirst (rrErrorStatus,rrErrorKind) sks of
                  Just (resErrStatus_,resErrKind_) ->
                      (True, Just $ RunResult{resDirectory_ = path
                                              ,resErrKind_
                                              ,resErrStatus_})

                  Nothing -> (False, Just $
                                   RunResult{resDirectory_ = path
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

                  anyFirst skIn@(sIn, kIn) sks =
                    if any (\(_,(ss,kk))-> ss==sIn && kindsEqual kk kIn) sks then
                        Just skIn
                    else
                        Nothing


            sameError (Right (_, SettingI{successful_=False,data_=SolveM ms })) =
                let
                    f ResultI{erroed= Just index, results } =
                        let ix = results `at` index
                        in Just (status_ ix, kind_ ix)
                    f _ = Nothing

                    sks = M.toList $  M.mapMaybe f ms

                in case anyFirst (rrErrorStatus,rrErrorKind) sks of
                   Just (resErrStatus_,resErrKind_)   ->
                       (True, Just $ RunResult{resDirectory_ = path
                                              ,resErrKind_
                                              ,resErrStatus_})
                   Nothing -> (False, Just $
                                    RunResult{resDirectory_ = path
                                             ,resErrKind_   = fstKind sks
                                             ,resErrStatus_ = fstStatus sks})

                where
                  anyFirst (StatusAny_,KindAny_) ((_,(x,y)):_) = Just (x,y)
                  anyFirst (StatusAny_,ki) v@((_,(x,_)):_) =
                      if any (\(_,(_,k)) -> kindsEqual k ki ) v then
                          Just (x,ki)
                      else
                          Nothing

                  anyFirst (si,KindAny_) v@((_,(_,y)):_) =
                      if any (\(_,(s,_)) -> s == si ) v then
                          Just (si,y)
                      else
                          Nothing

                  anyFirst skIn@(sIn, kIn) sks =
                    if any (\(_,(ss,kk))-> ss==sIn && kindsEqual kk kIn) sks then
                        Just skIn
                    else
                        Nothing



            sameError _ = (False, Nothing)

            fstStatus []            = error "fstStatus no statuses"
            fstStatus ((_,(s,_)):_) = s

            fstKind []            = error "fstKind no kinds"
            fstKind ((_,(_,k)):_) = k



        let stillErroed  = sameError res

        liftIO $ print $ ("Hasr rError?" :: String, fst stillErroed)
        liftIO $ putStrLn "\n\n"
        case stillErroed of
          (True, Just r)   -> return $ Just $ r
          (True, Nothing)  -> rrError "Same error but no result" []
          (False, Just r)  -> do
            -- liftIO $ putStrLn . show $ res
            -- liftIO $ putStrLn $ groom ("hasResult:" :: String,  pretty r)
            addOtherError r
            return Nothing

          (False, Nothing) -> do
             -- liftIO $ putStrLn . show $ res
             -- liftIO $ print $ ("noResult")
             return Nothing



modelRefineError :: KindI -> Bool
modelRefineError RefineCompact_ = True
modelRefineError RefineRandom_  = True
modelRefineError RefineAll_     = True
modelRefineError KindAny_       = True
modelRefineError _              = False

kindsEqual :: KindI -> KindI -> Bool
kindsEqual KindAny_ _ = True
kindsEqual _ KindAny_ = True

kindsEqual RefineCompact_ RefineAll_ = True
kindsEqual RefineRandom_  RefineAll_ = True

kindsEqual RefineAll_ RefineCompact_  = True
kindsEqual RefineAll_ RefineRandom_   = True

kindsEqual a b = a == b

addOtherError :: RunResult -> RR ()
addOtherError r = do
  return ()
  modify $ \st -> st{otherErrors_ =r : otherErrors_ st }
