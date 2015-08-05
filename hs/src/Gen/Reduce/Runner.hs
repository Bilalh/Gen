{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             DeriveTraversable, ViewPatterns #-}
module Gen.Reduce.Runner where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Gen.Classify.Meta     (mkMeta)
import Gen.Helpers.Log
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.RunResult
import Gen.IO.Toolchain      hiding (ToolchainData (..))
import Gen.Reduce.Data
import Gen.Reduce.TypeCheck
import GHC.Real              (floor)

import qualified Data.Map         as M
import qualified Gen.IO.Toolchain as Toolchain


data Timed a = NoTimeLeft a
             | Continue a
  deriving (Eq, Ord, Show, Read, Data, Foldable, Functor, Generic, Traversable, Typeable)

instance Pretty a => Pretty (Timed a) where
    pretty (Continue a)   = "Continue"   <+> (pretty a)
    pretty (NoTimeLeft a) = "NoTimeLeft" <+> (pretty a)

timedExtract :: Timed a -> a
timedExtract (Continue a)   = a
timedExtract (NoTimeLeft a) = a


timedSpec :: Spec
          -> (Maybe ErrData -> RR a)          -- No time left
          -> (Maybe ErrData -> RR (Timed a))  -- Time left
          -> RR (Timed a)
timedSpec sp f g = timedSpec2 runSpec sp f g

timedCompactSpec :: Spec
                 -> (Maybe ErrData -> RR a)          -- No time left
                 -> (Maybe ErrData -> RR (Timed a))  -- Time left
                 -> RR (Timed a) -- True means a similar error  still occured
timedCompactSpec = timedSpec2 (runSpec2 refineWay)

  where
    refineWay :: Maybe FilePath -> KindI -> RefineType
    refineWay Nothing  RefineCompact_ = Refine_Only
    refineWay Nothing  RefineRandom_  = Refine_Only
    refineWay (Just _) RefineCompact_ = Refine_Only
    refineWay (Just _) RefineRandom_  = Refine_Only
    refineWay (Just _) _              = Refine_Solve
    refineWay _        _              = Refine_Solve


timedSpec2 :: (Spec -> RR (Maybe ErrData, Int) )
           -> Spec
           -> (Maybe ErrData -> RR a)          -- No time left
           -> (Maybe ErrData -> RR (Timed a))  -- Time left
           -> RR (Timed a)
timedSpec2 runner sp f g= do
    -- xdb <- getsDb
    -- liftIO $ putStrLn $ "%DB:" ++ groom xdb
    startTime <- liftIO $ round `fmap` getPOSIXTime
    (res, cpuTimeUsed) <- runner sp
    endTime <- liftIO $ round `fmap` getPOSIXTime
    let realTimeUsed = endTime - startTime

    timeUsed <- gets rconfig >>= return . totalIsRealTime_ >>= \case
                True  -> return realTimeUsed
                False -> return cpuTimeUsed

    modify $ \st -> st{timeLeft_ = fmap (\x -> x - timeUsed )  (timeLeft_ st) }

    gets timeLeft_ >>= \case
         Just r   -> liftIO $ putStrLn $  "# " ++  (show (max r 0) ) ++ " seconds left"
         Nothing  -> return ()

    let process (Just a) b | a < b = do
          inner <- f res
          return $ (NoTimeLeft inner)
        process _ _ = g res

    get >>= \RState{timeLeft_,rconfig=RConfig{specTime_}} -> process timeLeft_ specTime_



-- Just means a similar error  still occured
runSpec :: (MonadDB m, MonadIO m, Applicative m, Functor m, HasLogger m, HasGen m, MonadR m)
        => Spec
        -> m (Maybe ErrData, Int)
runSpec spE =
  let refineWay :: Maybe FilePath -> KindI -> RefineType
      refineWay Nothing  RefineCompact_ = Refine_All
      refineWay Nothing  RefineRandom_  = Refine_All
      refineWay (Just _) RefineCompact_ = Refine_Only
      refineWay (Just _) RefineRandom_  = Refine_Only
      refineWay (Just _) _              = Refine_Solve
      refineWay _        _              = Refine_Solve_All
  in runSpec2 refineWay spE


runSpec2 :: (MonadDB m, MonadIO m, Applicative m, Functor m, HasLogger m, HasGen m, MonadR m)
        => (Maybe FilePath -> KindI -> RefineType)
        -> Spec
        -> m (Maybe ErrData, Int)
runSpec2 refineWay spE = do
  liftIO $ logSpec spE

  RConfig{..} <- getRconfig

  checkDB oErrKind_ oErrStatus_ spE >>= \case
    Just StoredError{} -> rrError "StoredResult in runSpec" []
    Just rr@Passing{} -> do
      liftIO $ putStrLn "? Using Cached data"
      showrrError rr
      processPassing spE
      return (Nothing, 0)
    Just rr@(OurError r)  -> do
      liftIO $  putStrLn "? Using Cached data"
      showrrError rr
      return $ (Just r, 0)

    Nothing -> do
      sp <- liftIO $ toModel spE

      ts <- liftIO $ timestamp >>= return . show
      -- My laptop is too fast
      ts_num <- chooseR (1000 :: Int, 9999) >>= return . show

      let path = outputDir_ </> (ts ++ "_" ++ ts_num)
      liftIO $ createDirectoryIfMissing True  path
      liftIO $ writeToJSON (path </> "spec.spec.json" )  spE

      meta <- mkMeta spE
      -- liftIO $  writeFile    (path </> "spec.meta" ) (show meta)
      liftIO $ writeToJSON  (path </> "spec.meta.json" ) (meta)

      liftIO $ Toolchain.copyMetaToSpecDir outputDir_ path

      seed        <- chooseR (0, 2^(24 :: Int))
      essencePath <- writeModelDef path sp
      choices     <- getChoicesToUse


      (stillErroed, timeTaken) <- if oErrKind_ == TypeCheck_ then do
        typeCheckWithResult path sp
      else do
        (_, res)  <- toolchain Toolchain.ToolchainData{
                      Toolchain.essencePath       = essencePath
                    , Toolchain.toolchainTime     = specTime_
                    , Toolchain.essenceParam      = Nothing
                    , Toolchain.refineType        = refineWay choices  oErrKind_
                    , Toolchain.cores             = cores_
                    , Toolchain.seed              = Just seed
                    , Toolchain.binariesDirectory = binariesDirectory_
                    , Toolchain.oldConjure        = False
                    , Toolchain.toolchainOutput   = toolchainOutput_
                    , Toolchain.choicesPath       = choices
                    , Toolchain.outputDirectory   = path
                    , Toolchain.dryRun            = False
                    }
        let timeTaken = floor $ Toolchain.getRunTime res


        addLog "runSpec" [pretty spE]
        addLog "runSpec_results" [nn "org_kind"   oErrKind_
                                 ,nn "org_status" oErrStatus_
                                 ,nn "res" (pretty . groom $ res)]

        let
            sameError :: ToolchainResult -> IO (Bool,RunResult)
            sameError e@(RefineResult SettingI{data_=RefineMap _}) = do
              error . show . vcat $ [ "Got back a result with no log following"
                                    , (pretty . groom) e
                                    ]


            sameError (RefineResult SettingI{successful_=False
                        ,data_=RefineMultiOutput{choices_made,cmd_used=CmdI{..}}})
                | modelRefineError oErrKind_ = do
                case match (oErrStatus_,oErrKind_) (status_, kind_) of
                  Just (status, kind) ->
                    return (True, OurError $ ErrData{ specDir = path
                                                    , kind
                                                    , status
                                                    , choices=choices_made
                                                    , timeTaken})

                  Nothing ->
                    return (False, OurError $ ErrData{ specDir = path
                                                     , kind    = kind_
                                                     , status  = status_
                                                     , choices = choices_made
                                                     , timeTaken})

                where
                  match :: (StatusI, KindI) -> (StatusI, KindI) -> Maybe (StatusI, KindI)
                  match (StatusAny_,KindAny_) (x,y)   = Just (x, y)
                  match (StatusAny_, ki)      (x,_)   = Just (x,ki)
                  match (si,KindAny_) (s,_) | s /= si = Nothing
                  match (si,KindAny_) (_,y) = Just (si,y)
                  match (si,ki) (x,y) =
                      if si == x && kindsEqual ki y  then
                          Just (si,ki)
                      else
                          Nothing


            sameError (SolveResult (_, SettingI{successful_=False,data_=SolveM ms,outdir_})) = do
                let
                    f ResultI{erroed= Just index, results } =
                        let ix = results `at` index
                        in Just (status_ ix, kind_ ix)
                    f _ = Nothing

                    sks = M.toList $  M.mapMaybe f ms
                c <- choicesUsed
                case anyFirst (oErrStatus_,oErrKind_) sks of
                   Just (status,kind)   ->
                     return (True, OurError $ ErrData { specDir = path
                                                      , kind
                                                      , status
                                                      , choices=c
                                                      , timeTaken})
                   Nothing -> return
                    (False, OurError $ ErrData {specDir = path
                                               ,kind    = fstKind sks
                                               ,status  = fstStatus sks
                                               ,choices = c
                                               ,timeTaken})

                where
                  choicesUsed = do
                      sizes <- forM (M.keys ms) $ \ep -> do
                          let choicesPath = outdir_ </> ep <.> ".eprime"
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



            sameError _ = return (False, Passing timeTaken)

            fstStatus []            = error "fstStatus no statuses"
            fstStatus ((_,(s,_)):_) = s

            fstKind []            = error "fstKind no kinds"
            fstKind ((_,(_,k)):_) = k

        stillErroed <- liftIO $ sameError res
        return (stillErroed, timeTaken)

      storeInDB spE (snd stillErroed)

      case (snd stillErroed) of
        (OurError ed) -> do
          liftIO $ writeToJSON (path </> "dir_error.json")
            Toolchain.DirError{ dirStatus = status ed , dirKind  = kind ed}

        _ -> return ()

      showrrError (snd stillErroed)

      case stillErroed of
        (True, Passing{})  -> rrError "Same error but no result" []
        (True, OurError r)  -> return ( Just r, timeTaken)

        (False, Passing{}) -> do
           case deletePassing_ of
                False -> return (Nothing, timeTaken)
                True  -> do
                    liftIO $ removeDirectoryRecursive path
                    return (Nothing, timeTaken)
        (False,(OurError r))  -> do
          processOtherError r
          return (Nothing, timeTaken)

        tu -> docError [ pretty $line
                       , "Invaild stillErroed"
                       , pretty tu]


showrrError :: (MonadR m, MonadIO m) => RunResult -> m ()
showrrError x = do
  case x of
    Passing{}        -> out $ "? Has rrError:" <+> pretty False
    (OurError ed)    -> f ed
    (StoredError ed) -> f ed
  liftIO $ putStrLn "\n"

  where
    out = liftIO  . putStrLn . show . pretty

    f ErrData{..} = do
      RConfig{..} <- getRconfig
      let b = (oErrKind_, oErrStatus_) == (kind, status)
      out . hang ("? Has rrError: " <+> pretty b) 4 . vcat  $ [
          nn "kind:   "  kind  <+>
             if kind /= oErrKind_ then "/=" <+> pretty oErrKind_ else ""
        , nn "status: " status <+>
             if status /= oErrStatus_ then "/=" <+> pretty oErrStatus_ else ""
        ]


modelRefineError :: KindI -> Bool
modelRefineError RefineCompact_ = True
modelRefineError RefineRandom_  = True
modelRefineError RefineAll_     = True
modelRefineError KindAny_       = True
modelRefineError _              = False

kindsEqual :: KindI -> KindI -> Bool
kindsEqual KindAny_ _ = True
kindsEqual _ KindAny_ = True

kindsEqual RefineAll_ RefineCompact_    = True
kindsEqual RefineAll_ RefineRandom_     = True

kindsEqual RefineCompact_ RefineAll_    = True
kindsEqual RefineCompact_ RefineRandom_ = True

kindsEqual RefineRandom_  RefineAll_     = True
kindsEqual RefineRandom_  RefineCompact_ = True


kindsEqual a b = a == b
