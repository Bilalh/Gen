module Gen.Essence.Generate(generateEssence) where

import Conjure.Language.Definition
import Conjure.UI.IO               (writeModel)
import Conjure.UI.TypeCheck        (typeCheckModel)
import Data.Time.Clock.POSIX       (getPOSIXTime)
import Gen.Classify.Meta           (mkMeta)
import Gen.Essence.Data            (EssenceConfig)
import Gen.IO.Formats
import Gen.IO.Toolchain            hiding (ToolchainData (..), copyMetaToSpecDir)
import Gen.Prelude
import GHC.Real                    (floor)
import System.Directory            (copyFile, renameDirectory)

import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Gen.Arbitrary.Arbitrary as Gen
import qualified Gen.Essence.Data        as EC
import qualified Gen.IO.Toolchain        as Toolchain


generateEssence :: EssenceConfig -> IO ()
generateEssence ec@EC.EssenceConfig{..} = do
  setRandomSeed seed_

  case mode_ of
    EC.TypeCheck_ -> doTypeCheck ec
    EC.Refine_    -> doRefine ec
    EC.Solve_     -> doSolve ec


  case deletePassing_ of
    False -> return ()
    True  -> do
      delete (outputDirectory_ </> "_passing")
      delete  (outputDirectory_ </> "_errors" </> "zPerSpec")

      where
        delete fp =
            doesDirectoryExist fp >>= \case
            False -> return ()
            True  -> removeDirectoryRecursive fp


doRefine :: EssenceConfig -> IO ()
doRefine ec@EC.EssenceConfig{..} = do
  process totalTime_

    where
    out    = outputDirectory_ </> "_passing"
    errdir = outputDirectory_ </> "_errors"

    process timeLeft | timeLeft <= 0 = return ()
    process timeLeft = do
      useSize <- (randomRIO (0, size_) :: IO Int)
      (sp,logs) <- generate $ Gen.spec useSize def{gen_useFunc = myUseFunc}
      model :: Model <- toConjure sp
      case ignoreLogs (typeCheckModel model)  of
        Left _ -> process timeLeft
        Right () -> do
          num <- (randomRIO (10,99) :: IO Int)  >>= return . show
          ts <- timestamp >>= return . show
          let uname  =  (ts ++ "_" ++ num )

          let dir = outputDirectory_ </> "_passing" </> uname
          createDirectoryIfMissing True dir
          writeFile (dir </> "spec.logs" ) (renderSized 120 logs)
          writeToJSON (dir </> "spec.spec.json") sp

          let meta = mkMeta sp
          writeFile (dir </> "spec.meta" ) (show meta)
          writeToJSON  (dir </> "spec.meta.json" ) (meta)
          Toolchain.copyMetaToSpecDir outputDirectory_ dir

          runSeed <- (randomRIO (1,2147483647) :: IO Int)
          essencePath <- writeModelDef dir model
          startTime <- round `fmap` getPOSIXTime
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
                    }
          endTime <- round `fmap` getPOSIXTime
          let realTime = endTime - startTime

          runTime <- classifySettingI ec errdir out uname result
          case totalIsRealTime of
            False -> process (timeLeft - (floor runTime))
            True  -> process (timeLeft - realTime)


doSolve :: EssenceConfig -> IO ()
doSolve ec@EC.EssenceConfig{..} = do
  process totalTime_

    where
    out    = outputDirectory_ </> "_passing"
    errdir = outputDirectory_ </> "_errors"

    process timeLeft | timeLeft <= 0 = return ()
    process timeLeft = do
      (sp,logs) <- generate $ Gen.spec size_ def{gen_useFunc = myUseFunc}
      model :: Model <- toConjure sp
      case ignoreLogs (typeCheckModel model)  of
        Left _ -> process timeLeft
        Right () -> do
          num <- (randomRIO (10,99) :: IO Int)  >>= return . show
          ts <- timestamp >>= return . show
          let uname  =  (ts ++ "_" ++ num )

          let dir = outputDirectory_ </> "_passing" </> uname
          createDirectoryIfMissing True dir
          writeFile (dir </> "spec.logs" ) (renderSized 120 logs)

          writeToJSON (dir </> "spec.spec.json") sp

          let meta = mkMeta sp
          writeFile (dir </> "spec.meta" ) (show meta)
          writeToJSON  (dir </> "spec.meta.json" ) (meta)
          Toolchain.copyMetaToSpecDir outputDirectory_ dir


          runSeed <- (randomRIO (1,2147483647) :: IO Int)
          essencePath <- writeModelDef dir model
          startTime <- round `fmap` getPOSIXTime
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
                    }

          endTime <- round `fmap` getPOSIXTime
          let realTime = endTime - startTime

          runTime <-  classifyError uname result
          case totalIsRealTime of
            False -> process (timeLeft - (floor runTime))
            True  -> process (timeLeft - realTime)


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

                return mvDir


            f _ _ = return ""

        void $ M.traverseWithKey f ms

        case EC.deletePassing_ ec of
          False -> return ()
          True  -> do
            removeDirectoryRecursive (inErrDir)

        return time_taken_

    classifyError uname (SolveResult (_, SettingI{time_taken_ })) = do
      case deletePassing_ of
        False -> return ()
        True  -> removeDirectoryRecursive (out </> uname)
      return time_taken_



classifySettingI :: EssenceConfig
                 -> FilePath
                 -> FilePath
                 -> FilePath
                 -> SettingI RefineM
                 -> IO Double -- timetaken
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

            return mvDir

    void $ M.traverseWithKey f ms

    case EC.deletePassing_ ec of
           False -> return ()
           True  -> do
             removeDirectoryRecursive (inErrDir)

    return time_taken_


classifySettingI ec _ out uname SettingI{time_taken_}  = do
  case EC.deletePassing_ ec of
    False -> return ()
    True  -> do
      removeDirectoryRecursive (out </> uname)

  return time_taken_


doTypeCheck :: EssenceConfig -> IO ()
doTypeCheck EC.EssenceConfig{..}= do
  process

  where
    process = do
      (sp,_) <- generate $ Gen.spec size_ def{gen_useFunc = myUseFunc}
      model :: Model <- toConjure sp


      let (res :: Either Doc ())  =ignoreLogs $ typeCheckModel model
      handleResult sp model res
      process


    handleResult sp model (Left d) = do
      putStrLn . show . pretty $ model
      putStrLn . show $ d
      writeFiles sp model d

    handleResult _ _ (Right _) = do
      return ()


    writeFiles ::Spec -> Model -> Doc -> IO ()
    writeFiles sp model errDoc = do
      num <- (randomRIO (10,99) :: IO Int)  >>= return . show
      ts <- timestamp >>= return . show
      let dir = outputDirectory_ </> "_typecheck" </> (ts ++ "_" ++ num)

      createDirectoryIfMissing True dir
      writeToJSON (dir </> "spec.spec.json") sp
      writeModel (Just (dir </> "spec.essence") ) model

      writeFile (dir </> "spec.error") $ show . vcat $
                    [ errDoc
                    , "----"
                    , pretty model
                    ]


allow :: String -> FilePath -> Bool
allow k f
    | k `isPrefixOf` f       = True
    | "json" `isSuffixOf` f  = True
    | "param" `isSuffixOf` f = True
    | "spec" `isPrefixOf` f  = True
    | "_" `isPrefixOf` f     = True
    | otherwise              = False


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

    "solve_eprime.json" -> do
       readFromJSON (inn </> g) >>= \case
         Nothing -> error $ "Could not parse json of : "  ++ (inn </> g)
         Just (ss@SettingI{data_=SolveM ms } ) -> do
           let ms' =  M.filterWithKey (\k _ ->  k `S.member` names) ms
               ss'      = ss{data_=SolveM ms'}
           writeToJSON (out </> g) ss'

    _ ->  copyFile (inn </> g) (out </> g)



-- Does not work completely
myUseFunc :: FuncsNames -> Bool
myUseFunc Aapply = False
myUseFunc Ahist = False
myUseFunc Ainverse = False
myUseFunc _ = True
