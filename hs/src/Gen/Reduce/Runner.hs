module Gen.Reduce.Runner where

import Gen.Classify.Meta        (mkMeta)
import Gen.IO.Formats
import Gen.IO.Toolchain         hiding (ToolchainData (..))
import Gen.Prelude
import Gen.Reduce.Data
import Gen.Reduce.FormatResults
import System.FilePath          (takeBaseName, replaceDirectory)
import System.Posix             (getFileStatus)
import System.Posix.Files       (fileSize)

import qualified Data.HashMap.Strict as H
import qualified Data.Map            as M
import qualified Gen.IO.Toolchain    as Toolchain



-- Just means rrError still happens
runSpec :: Spec -> RR (Maybe RunResult)
runSpec spE = do
  liftIO $ logSpec spE

  checkDB spE >>= \case
    Just StoredError{} -> rrError "StoredResult in runSpec" []
    Just Passing -> do
      liftIO $ print $ ("Stored no rrError"  :: String)
      liftIO $ putStrLn ""
      return Nothing
    Just r@OurError{}  -> do
      liftIO $ print $ ("Stored has rrError" :: String)
      liftIO $ putStrLn ""
      return $ Just r

    Nothing -> do
      sp <- liftIO $ toModel spE
      outdir <- gets outputDir_

      ts <- liftIO $ timestamp >>= return . show
      -- My laptop is too fast
      ts_num <- chooseR (1000 :: Int, 9999) >>= return . show

      let path = outdir </> (ts ++ "_" ++ ts_num)
      liftIO $ createDirectoryIfMissing True  path
      liftIO $ writeToJSON (path </> "spec.spec.json" )  spE

      let meta = mkMeta spE
      liftIO $  writeFile    (path </> "spec.meta" ) (show meta)
      liftIO $  writeToJSON  (path </> "spec.meta.json" ) (meta)

      liftIO $ Toolchain.copyMetaToSpecDir outdir path

      seed <- chooseR (0, 2^(24 :: Int))
      perSpec <- gets specTime_
      essencePath <- writeModelDef path sp
      cores <- gets Gen.Reduce.Data.cores_
      bd   <- gets binariesDirectory_
      oo   <- gets toolchainOutput_
      choices <- gets mostReducedChoices_

      let refineWay :: Maybe FilePath -> KindI -> RefineType
          --FIXME hadle Compact
          refineWay Nothing  RefineCompact_ = Refine_All
          refineWay Nothing  RefineRandom_  = Refine_All
          refineWay (Just _) RefineCompact_ = Refine_Only
          refineWay (Just _) RefineRandom_  = Refine_Only
          refineWay (Just _) _              = Refine_Solve
          refineWay _        _              = Refine_Solve_All

      rrErrorKind   <- gets oErrKind_
      rrErrorStatus <- gets oErrStatus_

      (_, res)  <- toolchain Toolchain.ToolchainData{
                    Toolchain.essencePath       = essencePath
                  , Toolchain.outputDirectory   = path
                  , Toolchain.toolchainTime     = perSpec
                  , Toolchain.essenceParam      = Nothing
                  , Toolchain.refineType        = refineWay choices  rrErrorKind
                  , Toolchain.cores             = cores
                  , Toolchain.seed              = Just seed
                  , Toolchain.binariesDirectory = bd
                  , Toolchain.oldConjure        = False
                  , Toolchain.toolchainOutput   = oo
                  , Toolchain.choicesPath       = choices
                  }


      addLog "runSpec" [pretty spE]
      addLog "runSpec_results" [nn "org_kind"   rrErrorKind
                               ,nn "org_status" rrErrorStatus
                               ,nn "res" (pretty . groom $ res)]

      let
          sameError :: ToolchainResult -> IO (Bool,Maybe RunResult)
          sameError (RefineResult SettingI{successful_=False, data_=RefineM ms,outdir_})
              | modelRefineError rrErrorKind = do
              let sks = M.toList $  M.map ( status_ &&& kind_) ms
              resErrChoices_ <- choicesUsed
              case anyFirst (rrErrorStatus,rrErrorKind) sks of
                Just (resErrStatus_,resErrKind_) ->
                  return (True, Just $ OurError{resDirectory_ = path
                                               ,resErrKind_
                                               ,resErrStatus_
                                               ,resErrChoices_})

                Nothing ->
                  return (False, Just $ OurError{resDirectory_ = path
                                                ,resErrKind_   = fstKind sks
                                                ,resErrStatus_ = fstStatus sks
                                                ,resErrChoices_})

              where
                choicesUsed = do
                  sizes <- forM (M.keys ms) $ \ep -> do
                      let choicesPath = outdir_
                                     </> replaceExtensions ep ".choices.json"
                      size <- getFileSize choicesPath
                      return (choicesPath, size)

                  let (minChoice,_)  = minimumBy (comparing snd) sizes
                  return minChoice

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



          sameError (SolveResult (_, SettingI{successful_=False,data_=SolveM ms,outdir_})) = do
              let
                  f ResultI{erroed= Just index, results } =
                      let ix = results `at` index
                      in Just (status_ ix, kind_ ix)
                  f _ = Nothing

                  sks = M.toList $  M.mapMaybe f ms
              resErrChoices_ <- choicesUsed
              case anyFirst (rrErrorStatus,rrErrorKind) sks of
                 Just (resErrStatus_,resErrKind_)   ->
                   return (True, Just $ OurError{resDirectory_ = path
                                                ,resErrKind_
                                                ,resErrStatus_
                                                ,resErrChoices_})
                 Nothing -> return
                  (False, Just $ OurError{resDirectory_ = path
                                         ,resErrKind_   = fstKind sks
                                         ,resErrStatus_ = fstStatus sks
                                         ,resErrChoices_})

              where
                choicesUsed = do
                    sizes <- forM (M.keys ms) $ \ep -> do
                        let choicesPath = outdir_ </> ep
                        size <- getFileSize choicesPath
                        return (choicesPath, size)

                    let (minChoice,_)  = minimumBy (comparing snd) sizes
                    return minChoice

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



          sameError _ = return (False, Nothing)

          fstStatus []            = error "fstStatus no statuses"
          fstStatus ((_,(s,_)):_) = s

          fstKind []            = error "fstKind no kinds"
          fstKind ((_,(_,k)):_) = k



      stillErroed  <- liftIO $ sameError res
      storeInDB spE (snd stillErroed)

      liftIO $ print $ ("Has rrError?" :: String, fst stillErroed)
      liftIO $ putStrLn "\n\n"

      case stillErroed of
        (True, Just r)   -> return $ Just $ r
        (True, Nothing)  -> rrError "Same error but no result" []
        (False, Just r)  -> do
          addOtherError r
          return Nothing

        (False, Nothing) -> do
           gets deletePassing_ >>= \case
                False -> return Nothing
                True  -> do
                     liftIO $ removeDirectoryRecursive path
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


giveDb :: Maybe FilePath -> IO ResultsDB
giveDb Nothing    = return $ H.empty
giveDb (Just dir) = do
  readFromJSONMay (dir </> "db.json") >>= \case
    Nothing  -> return $ H.empty
    (Just x) -> return x


-- | Check if the spec's hash is contained, return the result if it is
checkDB :: Spec -> RR (Maybe RunResult)
checkDB newE= do
  let newHash = hash newE
  gets resultsDB_ >>=  \m ->
      case newHash `H.lookup` m of
        Nothing              -> return Nothing
        Just r@Passing{}     -> return (Just r)
        Just r@OurError{}    -> return (Just r)
        Just StoredError{..} -> do
          out <- gets outputDir_
          let outDir = (out </> takeBaseName resDirectory_ )
          let newChoices = replaceDirectory resErrChoices_ outDir
          let err = OurError{resDirectory_=outDir, resErrChoices_=newChoices, .. }

          liftIO $ doesDirectoryExist outDir >>= \case
            True  -> return $ Just err
            False -> do
              liftIO $ copyDirectory resDirectory_  outDir
              return $ Just err


storeInDB :: Spec -> Maybe RunResult  -> RR ()
storeInDB sp mr = do
  let newHash = hash sp
  gets resultsDB_ >>=  \m -> do
      let r = case mr of
                Just v  -> v
                Nothing -> Passing
      let newDB = H.insert newHash r m
      modify $ \st -> st{resultsDB_ = newDB}


-- | Cache the results if given a filepath
saveDB :: Maybe FilePath -> ResultsDB -> IO ()
saveDB Nothing  _    = return ()
saveDB (Just dir) db = do
  createDirectoryIfMissing True dir
  ndb <- H.traverseWithKey f db
  writeToJSON (dir </> "db.json") ndb

  where
    f _ OurError{..} = do
      liftIO $ putStrLn ""
      let newDir = dir </> takeBaseName resDirectory_
      copyDirectory resDirectory_ newDir
      let newChoices = replaceDirectory resErrChoices_ newDir
      return $ StoredError{resDirectory_= newDir, resErrChoices_=newChoices, ..}

    f _ x = return x

getFileSize :: FilePath -> IO Integer
getFileSize path = getFileStatus
                   path >>= \s -> return $ fromIntegral $ fileSize s
