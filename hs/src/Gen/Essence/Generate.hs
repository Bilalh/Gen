module Gen.Essence.Generate(generateEssence) where

import Conjure.Language.Definition
import Conjure.Language.NameResolution (resolveNames)
import Conjure.UI.IO                   (EssenceFileMode (..), writeModel)
import Conjure.UI.TypeCheck            (typeCheckModel)
import Data.Time.Clock.POSIX           (getPOSIXTime)
import Gen.Classify.Meta               (mkMeta)
import Gen.Essence.Adjust
import Gen.Essence.Carry
import Gen.Essence.Reduce              (ErrData (..), reduceError, reduceErrors)
import Gen.Essence.Spec                ()
import Gen.Essence.St
import Gen.Essence.UIData              (EssenceConfig)
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.RunResult
import Gen.IO.Toolchain                hiding (DirError (..), ToolchainData (..))
import GHC.Real                        (floor)
import System.Directory                (copyFile, renameDirectory)
import Test.QuickCheck                 (Gen, generate)

import qualified Data.IntSet             as I
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Gen.Arbitrary.Arbitrary as FirstGen
import qualified Gen.Arbitrary.Data      as FirstGen
import qualified Gen.Essence.UIData      as EC
import qualified Gen.IO.Toolchain        as Toolchain



generateEssence :: KeyMap -> EssenceConfig -> IO ()
generateEssence km ec@EC.EssenceConfig{..} = do
  setRandomSeed seed_
  let carry = Carry{ cHashes            = runHashes_
                   , cWeighting         = km
                   , cWeightingHashPrev = 0
                   , cDB                = def
                   , cSpecDir           = outputDirectory_ </> "_errors"
                   , cDBDir             = outputDirectory_ </> "cache"
                   }
  case mode_ of
    EC.TypeCheck_ -> void $ evalStateT (doTypeCheck ec) carry
    EC.Refine_    -> void $ evalStateT (doRefine ec) carry
    EC.Solve_     -> void $ evalStateT (doSolve ec) carry


  case deletePassing_ of
    False -> return ()
    True  -> do
      delete (outputDirectory_ </> "_passing")
      delete (outputDirectory_ </> "_errors" </> "zPerSpec")

      where
        delete fp =
            doesDirectoryExist fp >>= \case
            False -> return ()
            True  -> removeDirectoryRecursive fp


generateWrap :: MonadIO m => Maybe [FilePath] -> Gen (Spec, Doc) -> m (Spec, Doc)
generateWrap (Just [])  _ =
    $(neverNote "generateWrap No given specs left")

generateWrap (Just (x:_)) _ = do
  spec :: Spec <- liftIO $ readFromJSON x
  return (spec, "")

generateWrap _ f = liftIO $  generate f


doRefine :: (Applicative m, MonadIO m, MonadState Carry m, MonadDB m)
         => EssenceConfig -> m ()
doRefine ec@EC.EssenceConfig{..} = do
  process totalTime_ givenSpecs_

    where
    out    = outputDirectory_ </> "_passing"
    errdir = outputDirectory_ </> "_errors"

    process :: (Applicative m, MonadIO m, MonadState Carry m, MonadDB m)
            => Int -> Maybe [FilePath]  -> m ()
    process timeLeft Nothing  | timeLeft <= 0 = return ()
    process _ (Just [])  = return ()

    process timeLeft mayGiven = do
      case mayGiven of
          Nothing -> liftIO $ putStrLn $  "# " ++  (show (max timeLeft 0) ) ++ " seconds left"
          Just ys -> liftIO $ putStrLn $  "# " ++  (show (length ys) ) ++ " specs left"

      useSize <- liftIO $ ( randomRIO (0, size_) :: IO Int)
      gen <-  genToUse useSize ec
      (sp,logs) <- generateWrap mayGiven $ gen

      gets cHashes >>= \h -> case (hash sp) `I.member` h of
        True -> do
          liftIO $ putStrLn $ "Not running spec with hash, already tested " ++ (show $ hash sp)
          process (timeLeft) (nextElem mayGiven)
        False -> do
          liftIO $ putStrLn $ "> Processing: " ++ (show $ hash sp)
          modify $ \st -> st{cHashes= (hash sp) `I.insert` h}
          let model :: Model = toConjureNote "Generate toConjure" sp
          case typeCheck model  of

            Left x ->
                case givenSpecs_ of
                  Just{} ->
                    error . show . vcat $
                        [ "Spec failed type checking"
                        , pretty model
                        , pretty x
                        , pretty . groom $ model
                        ]
                  Nothing -> process timeLeft (nextElem mayGiven)

            Right{} ->  do
              num <- liftIO $ (randomRIO (10,99) :: IO Int)  >>= return . show
              ts <- timestamp >>= return . show
              let uname  =  (ts ++ "_" ++ num )

              let dir = outputDirectory_ </> "_passing" </> uname
              liftIO $ createDirectoryIfMissing True dir
              liftIO $ writeFile (dir </> "spec.logs" ) (renderSized 120 logs)
              liftIO $ writeToJSON (dir </> "spec.spec.json") sp
              gets cWeighting >>= \c -> liftIO $ writeToJSON (dir </> "weighting.json") c



              meta <- mkMeta sp
              liftIO $ writeToJSON  (dir </> "spec.meta.json" ) (meta)
              Toolchain.copyMetaToSpecDir outputDirectory_ dir

              runSeed <- liftIO $ (randomRIO (1,2147483647) :: IO Int)
              essencePath <- writeModelDef dir model
              startTime <- liftIO $ round `fmap` getPOSIXTime
              (_, RefineResult result) <- toolchain Toolchain.ToolchainData{
                          Toolchain.essencePath       = essencePath
                        , Toolchain.outputDirectory   = dir
                        , Toolchain.toolchainTime     = perSpecTime_
                        , Toolchain.essenceParam      = Nothing
                        , Toolchain.refineType        = Refine_Only
                        , Toolchain.cores             = cores_
                        , Toolchain.seed              = Just runSeed
                        , Toolchain.binariesDirectory = binariesDirectory_
                        , Toolchain.oldConjure        = oldConjure_
                        , Toolchain.toolchainOutput   = toolchainOutput_
                        , Toolchain.choicesPath       = Nothing
                        , Toolchain.dryRun            = False
                        }
              endTime <- liftIO $ round `fmap` getPOSIXTime
              let realTime = endTime - startTime

              (runTime,rdata) <- liftIO $ classifySettingI ec errdir out uname result

              case reduceAsWell_ of
                Nothing -> return ()
                Just{}  -> do
                  liftIO $ putStrLn $ "> Reducing: " ++ (show $ hash sp)
                  res <- reduceErrors ec rdata
                  liftIO $ putStrLn $ "> Reduced: " ++ (show $ hash sp)
                  liftIO $ putStrLn $ "!l rdata: " ++ (show $ length rdata)
                  liftIO $ putStrLn $ "! rdata: " ++ (show $ vcat $ map pretty rdata)
                  liftIO $ putStrLn $ "!l errData: " ++ (show $ length res)
                  liftIO $ putStrLn $ "! errData: " ++ (show $ vcat $ map pretty res)
                  mapM_ adjust res



              liftIO $ putStrLn $ "> Processed: " ++ (show $ hash sp)
              liftIO $ putStrLn $ ""
              case totalIsRealTime of
                False -> process (timeLeft - (floor runTime)) (nextElem mayGiven)
                True  -> process (timeLeft - realTime) (nextElem mayGiven)


doSolve :: (Applicative m, MonadIO m, MonadState Carry m, MonadDB m)
        => EssenceConfig -> m ()
doSolve ec@EC.EssenceConfig{..} = do

  process totalTime_ givenSpecs_

    where
    out    = outputDirectory_ </> "_passing"
    errdir = outputDirectory_ </> "_errors"

    process :: (Applicative m, MonadIO m, MonadState Carry m, MonadDB m)
          => Int -> Maybe [FilePath] -> m ()
    process timeLeft Nothing  | timeLeft <= 0 = return ()
    process _ (Just [])  = return ()

    process timeLeft mayGiven  = do
      useSize <- liftIO $ (randomRIO (0, size_) :: IO Int)
      gen <-  genToUse useSize ec
      (sp,logs) <- generateWrap mayGiven $ gen

      gets cHashes >>= \h -> case (hash sp) `I.member` (h) of
        True -> do
          liftIO $ putStrLn $ "Not running spec with hash, already tested " ++ (show $ hash sp)
          process (timeLeft) (nextElem mayGiven)
        False -> do
          modify $ \st -> st{cHashes= (hash sp) `I.insert` h}
          let model :: Model = toConjureNote "Generate toConjure" sp
          case typeCheck model  of
            Left x ->
                case givenSpecs_ of
                  Just{} ->
                    error . show . vcat $
                        [ "Spec failed type checking"
                        , pretty model
                        , pretty x
                        , pretty . groom $ model
                        ]
                  Nothing -> process timeLeft (nextElem mayGiven)

            Right{} -> do
              num <- liftIO (randomRIO (10,99) :: IO Int)  >>= return . show
              ts <- timestamp >>= return . show
              let uname  =  (ts ++ "_" ++ num )

              let dir = outputDirectory_ </> "_passing" </> uname
              liftIO $ createDirectoryIfMissing True dir
              liftIO $ writeFile (dir </> "spec.logs" ) (renderSized 120 logs)

              liftIO $ writeToJSON (dir </> "spec.spec.json") sp

              meta <- mkMeta sp
              liftIO $  writeToJSON  (dir </> "spec.meta.json" ) (meta)
              Toolchain.copyMetaToSpecDir outputDirectory_ dir


              runSeed <- liftIO $  (randomRIO (1,2147483647) :: IO Int)
              essencePath <- writeModelDef dir model
              startTime <- liftIO $ round `fmap` getPOSIXTime
              (_, result) <- toolchain Toolchain.ToolchainData{
                          Toolchain.essencePath       = essencePath
                        , Toolchain.outputDirectory   = dir
                        , Toolchain.toolchainTime     = perSpecTime_
                        , Toolchain.essenceParam      = Nothing
                        , Toolchain.refineType        = Refine_Solve
                        , Toolchain.cores             = cores_
                        , Toolchain.seed              = Just runSeed
                        , Toolchain.binariesDirectory = binariesDirectory_
                        , Toolchain.oldConjure        = oldConjure_
                        , Toolchain.toolchainOutput   = toolchainOutput_
                        , Toolchain.choicesPath       = Nothing
                        , Toolchain.dryRun            = False
                        }
              endTime <- liftIO $  round `fmap` getPOSIXTime
              let realTime = endTime - startTime

              (runTime,rdata) <- liftIO $  classifyError uname result

              case reduceAsWell_ of
                Nothing -> return ()
                Just{}  -> do
                  liftIO $ putStrLn $ "> Reducing: " ++ (show $ hash sp)
                  res <- reduceErrors ec rdata
                  liftIO $ putStrLn $ "> Reduced: " ++ (show $ hash sp)
                  liftIO $ putStrLn $ "!l rdata: " ++ (show $ length rdata)
                  liftIO $ putStrLn $ "! rdata: " ++ (show $ vcat $ map pretty rdata)
                  liftIO $ putStrLn $ "!l errData: " ++ (show $ length res)
                  liftIO $ putStrLn $ "! errData: " ++ (show $ vcat $ map pretty res)
                  mapM_ adjust res

              case totalIsRealTime of
                False -> process (timeLeft - (floor runTime)) (nextElem mayGiven)
                True  -> process (timeLeft - realTime) (nextElem mayGiven)


    classifyError uname (RefineResult a) = classifySettingI ec errdir out uname a

    classifyError uname (SolveResult (_,
          ee@SettingI{successful_=False,data_=SolveM ms,time_taken_})) = do

        let inErrDir = errdir </> "zPerSpec" </> uname
        createDirectoryIfMissing True inErrDir
        renameDirectory (out </> uname ) inErrDir

        rr <- flip M.traverseWithKey (M.filter (isJust . erroed ) ms ) $
             \_ k -> case k of
                       ResultI{last_status, last_kind=Just kind, erroed= Just _ } -> do
                              let mvDirBase = errdir </> (show kind) </> (show last_status)
                              return $ Just mvDirBase

                       _  -> return Nothing


        let inDir = M.map S.fromList
                  . M.fromListWith (\a b -> a ++ b)
                  . map (\(a,b) -> (b, [a]))
                  . M.toList
                  $  M.mapMaybe id rr


        let
            unMaybe (Just a) = a
            unMaybe Nothing = docError $ ["unMaybe: classifyError"
                                         , nn "uname" uname
                                         , nn "ee" (show ee)
                                         , nn "ee" (show rr)
                                         , nn "ee" (show inDir)]

            f k ResultI{last_status, last_kind=Just kind, erroed= Just _ } = do
                let mvDirBase = errdir </> (show kind) </> (show last_status)
                let mvDir     = mvDirBase </> uname
                createDirectoryIfMissing True mvDir
                writeToJSON (mvDir </> "dir_error.json") Toolchain.DirError{dirStatus =last_status, dirKind = kind}

                fps <- getDirectoryContents inErrDir
                let needed =  filter (allow k) fps

                void $ copyFiles (unMaybe $ mvDirBase `M.lookup` inDir) inErrDir mvDir needed

                case (kind,last_status) `S.member` (EC.notUseful ec)  of
                  False -> return ()
                  True  -> do
                    putStrLn . show . vcat $ [
                                    "Deleting " <+> (pretty . groom $ (kind,last_status) )
                                  ,  "mvDir"   <+> pretty mvDir
                                  ]
                    removeDirectoryRecursive mvDir

                let err = ErrData { kind    = kind
                                  , status  = last_status
                                  , choices = mvDir </> k <.> ".choices.json"
                                  , specDir = mvDir
                                  }
                return $ Just err


            f _ _ = return Nothing

        err <-  M.traverseWithKey f ms

        case EC.deletePassing_ ec of
          False -> return ()
          True  -> do
            removeDirectoryRecursive (inErrDir)

        return (time_taken_, [ v | (_,Just v) <- M.toList err])

    classifyError uname (SolveResult (_, SettingI{time_taken_ })) = do
      case deletePassing_ of
        False -> return ()
        True  -> removeDirectoryRecursive (out </> uname)
      return (time_taken_, [])



classifySettingI :: EssenceConfig
                 -> FilePath
                 -> FilePath
                 -> FilePath
                 -> SettingI RefineM
                 -- -> IO Double -- timetaken
                 -> IO (Double, [(ErrData)]) -- timetaken
classifySettingI ec errdir out uname
                 ee@SettingI{successful_=False,data_=RefineMap ms,time_taken_} = do
    let inErrDir = errdir </> "zPerSpec" </> uname
    createDirectoryIfMissing True inErrDir
    renameDirectory (out </> uname ) inErrDir


    rr <- flip M.traverseWithKey ms $
         \_ CmdI{status_, kind_ } -> do
           let mvDirBase = errdir </> (show kind_) </> (show status_)
           return $ mvDirBase

    let inDir = M.map S.fromList
              . M.fromListWith (\a b -> a ++ b)
              . map (\(a,b) -> (b, [a]))
              . M.toList
              $ rr

    let
        unMaybe (Just a) = a
        unMaybe Nothing = docError $ ["unMaybe: classifySettingI"
                                     , nn "uname" uname
                                     , nn "ee" (show ee)
                                     , nn "ee" (show rr)
                                     , nn "ee" (show inDir)]

        f k CmdI{status_, kind_ } = do
            let mvDirBase = errdir </> (show kind_) </> (show status_)
            let mvDir = mvDirBase </> uname

            createDirectoryIfMissing True mvDir
            writeToJSON (mvDir </> "dir_error.json")
                        Toolchain.DirError{dirStatus =status_, dirKind = kind_ }

            fps <- getDirectoryContents inErrDir
            let needed =  filter (allow k) fps

            void $ copyFiles (unMaybe $ mvDirBase `M.lookup` inDir) inErrDir mvDir needed

            case (kind_,status_) `S.member` (EC.notUseful ec)  of
              False -> return ()
              True  -> do
                putStrLn . show . vcat $ [
                                "Deleting " <+> (pretty . groom $ (kind_,status_) )
                              ,  "mvDir"   <+> pretty mvDir
                              ]
                removeDirectoryRecursive mvDir

            let err = ErrData { kind = kind_
                              , status = status_
                              , choices = mvDir </> k <.> ".choices.json"
                              , specDir = mvDir
                              }
            return err

    err <- M.traverseWithKey f ms

    case EC.deletePassing_ ec of
           False -> return ()
           True  -> do
             removeDirectoryRecursive (inErrDir)

    return (time_taken_,  [ v | (_,v) <- M.toList err] )


classifySettingI ec _ out uname SettingI{time_taken_}  = do
  case EC.deletePassing_ ec of
    False -> return ()
    True  -> do
      removeDirectoryRecursive (out </> uname)

  return (time_taken_, [])


typeCheck :: MonadFail m => Model -> m Model
typeCheck m = ignoreLogs . runNameGen  $ (resolveNames $ m) >>= typeCheckModel

doTypeCheck :: (MonadIO m, MonadState Carry m, MonadDB m)
            => EssenceConfig -> m ()
doTypeCheck ec@EC.EssenceConfig{..}= do
  _ <- process 0
  return ()
  -- Never gets called
  -- liftIO $ putStrLn $ case num of
  --   0 -> "No errors found"
  --   1 -> "A single Error found"
  --   n -> show n ++ " Errors found"

  where
    process :: (MonadIO m, MonadState Carry m, MonadDB m)
            => Int -> m Int
    process i = do
      useSize <- liftIO $ (randomRIO (0, size_) :: IO Int)
      gen <- genToUse useSize ec
      (sp,logs) <- liftIO $  generate $ gen
      let model :: Model = toConjureNote "Generate toConjure" sp


      let (res :: Either Doc Model) =  typeCheck  model
      handleResult logs sp model res
      case res of
        Right{} -> process i
        Left{}  -> process (i+1)


    handleResult logs sp model (Left d) = do
      when ( toolchainOutput_  /= ToolchainNull_ ) $ liftIO $ do
        putStrLn . show . pretty $ model
        putStrLn . show $ d

      num <- liftIO $ (randomRIO (10,99) :: IO Int)  >>= return . show
      ts <- liftIO $ timestamp >>= return . show
      let dir = outputDirectory_ </> "_typecheck" </> (ts ++ "_" ++ num)

      liftIO $ writeFiles dir sp model d
      liftIO $ writeFile (dir </> "model000000.choices.json") "{}"
      liftIO $ writeFile (dir </> "spec.logs" ) (renderSized 120 logs)


      case reduceAsWell_ of
        Nothing -> return ()
        Just{}  -> do
          let rdata = ErrData{kind=TypeCheck_
                             ,specDir=dir
                             ,status=TypeChecking_
                             ,choices=dir </> "model000000.choices.json"}
          liftIO $ putStrLn $ "> Reducing: " ++ (show $ hash sp)
          res <- reduceError ec rdata
          liftIO $ putStrLn $ "> Reduced: " ++ (show $ hash sp)
          adjust res


    handleResult _ _ _ (Right _) = do
      return ()


    writeFiles ::FilePath -> Spec -> Model -> Doc -> IO ()
    writeFiles dir sp model errDoc = do

      createDirectoryIfMissing True dir
      writeToJSON (dir </> "spec.spec.json") sp
      writeModel PlainEssence (Just (dir </> "spec.essence") ) model

      writeFile (dir </> "spec.error") $ show . vcat $
                    [ errDoc
                    , "----"
                    , pretty model
                    ]


allow :: String -> FilePath -> Bool
allow k f
    | k `isPrefixOf` f              = True
    | "choices.json" `isSuffixOf` f = False
    | "json" `isSuffixOf` f         = True
    | "csv" `isSuffixOf` f          = True
    | "param" `isSuffixOf` f        = True
    | "spec" `isPrefixOf` f         = True
    | "_" `isPrefixOf` f            = True
    | otherwise                     = False


copyFiles :: Traversable t =>
             Set String -> FilePath -> FilePath -> t FilePath -> IO (t ())
copyFiles names inn out needed = forM needed $ \g -> do
  case g of
    "refine_essence.json" -> do
       readFromJSON (inn </> g) >>= \case
         Nothing -> error $ "Could not parse json of : "  ++ (inn </> g)
         Just (ss@SettingI{data_=RefineMap ms } ) -> do
           let ms' =  M.filterWithKey (\k _ ->  k `S.member` names) ms
               ss'      = ss{data_=RefineMap ms'}
           writeToJSON (out </> g) ss'

         Just (ss) -> do
           error . show .  vcat $ ["copyFiles unhandled", nn "ss" (groom ss) ]

    "solve_eprime.json" -> do
       readFromJSON (inn </> g) >>= \case
         Nothing -> error $ "Could not parse json of : "  ++ (inn </> g)
         Just (ss@SettingI{data_=SolveM ms } ) -> do
           let ms' =  M.filterWithKey (\k _ ->  k `S.member` names) ms
               ss'      = ss{data_=SolveM ms'}
           writeToJSON (out </> g) ss'

    _ ->  copyFile (inn </> g) (out </> g)


nextElem :: Maybe [a] -> Maybe [a]
nextElem Nothing       = Nothing
nextElem (Just [])     = $(neverNote "nextElem No given specs left")
nextElem (Just (_:xs)) = Just xs

genToUse :: (MonadIO m, MonadState Carry m)
         => Depth -> EssenceConfig -> m (Gen (Spec,Doc))
genToUse depth EC.EssenceConfig{genType_=EC.SecondGen} = do
  oldHash <- gets cWeightingHashPrev
  ws <- gets cWeighting
  let hws = hash ws
  case hws /= oldHash of
    False -> return ()
    True -> do
      modify $ \st -> st{cWeightingHashPrev=hws}
      liftIO $ print $ hang "Weightings Changed" 4 (pretty ws)


  let g = f <$> runGenerateWithLogs GNone def{ depth=depth
                                             , weighting=ws}
  return g

  where
  allowed = LogDebug
  f :: (Spec,[(LogLevel,Doc)]) -> (Spec,Doc)
  f (sp,logs) = (sp, vcat [ msg | (lvl, msg) <- logs , lvl <= allowed ])

genToUse depth EC.EssenceConfig{genType_=EC.FirstGen} = do
 let g = f <$> FirstGen.spec depth def{FirstGen.gen_useFunc = myUseFunc}
 return g

  where
  f (sp,logs) = (sp, pretty logs)

  myUseFunc :: FirstGen.FuncsNames -> Bool
  myUseFunc FirstGen.Aapply   = False
  myUseFunc FirstGen.Ahist    = False
  myUseFunc FirstGen.Ainverse = False
  myUseFunc _                 = True
