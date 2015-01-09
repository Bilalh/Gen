{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, TupleSections #-}

{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-cse #-}
-- cse means output is not outputted

module TestGen.QC where

import AST.SpecE
import Language.E hiding(trace)
import Language.E.Pipeline.ReadIn(writeSpec)

import Common.Helpers(timestamp)

import TestGen.Arbitrary.Arbitrary
import TestGen.Helpers.Runner
import TestGen.Helpers.Args(TArgs(..))
import TestGen.Prelude(SpecState, Generators,Domain, listOfBounds,SS(depth_),FG ,ArbSpec(..))
import TestGen.Helpers.QuickCheck2(quickCheckWithResult2)

import Data.Time
import Data.Time.Clock.POSIX(getPOSIXTime)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Property as QC

import System.Directory(createDirectoryIfMissing, getHomeDirectory, doesFileExist
                       ,renameDirectory, removeDirectory,removeDirectoryRecursive
                       ,getDirectoryContents, copyFile)
import System.FilePath((</>), (<.>), takeFileName)
import System.IO(IOMode(..),hPutStrLn, openFile, hClose)
import System.Process(system)

import Test.QuickCheck
import Test.QuickCheck.Random(mkQCGen,QCGen)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Text.Groom(groom)

import TestGen.QCDebug

import Data.Traversable(for)

import System.Random(RandomGen(split),setStdGen, mkStdGen)

import Data.Maybe(fromJust)

import TestGen.Prelude(nn)



prop_specs_refine :: ArbSpec a =>
    WithExtra a -> Cores ->  Int -> FilePath -> Bool -> WithExtra a -> Property
prop_specs_refine  _ cores time out newConjure WithExtra{..} = do
    let specE = getSpec inner_
        sp    = toSpec specE
    fst (typeChecks sp) ==>
        monadicIO $ do
            ts <- run timestamp >>= return . show
            -- num <- run (randomRIO (10,99) :: IO Int)  >>= return . show
            let num = show ts_int_
            let uname  =  (ts ++ "_" ++ num )
            let outdir =  (out </> uname)
            run $ createDirectoryIfMissing True outdir
            run $  writeFile (outdir </> "spec.logs" ) (renderNormal wlogs_)
            run $  writeFile (outdir </> "spec.specE" ) (show specE)

            result <- run $ runRefine' run_seed_ cores sp (out </> uname ) time newConjure

            classifyError uname result

    where
    errdir  = out </> "_errors"
    classifyError uname (SettingI{successful_=False,data_=RefineM ms  }) = do
        let inErrDir = errdir </> "zall" </> uname
        run $ createDirectoryIfMissing True inErrDir
        run $ renameDirectory (out </> uname ) inErrDir


        let

            f k CmdI{status_, kind_ } = do
                let mvDir = errdir </> (show kind_) </> (show status_) </> uname
                createDirectoryIfMissing True mvDir
                fps <- getDirectoryContents inErrDir
                let needed =  filter (allow k) fps

                void $ forM needed $ \g -> do
                    copyFile (inErrDir </> g) (mvDir </> g)

                return mvDir

        void $ run $ M.traverseWithKey f ms

        fail inErrDir

    classifyError _ _ = return ()

prop_specs_toolchain :: ArbSpec a =>
    WithExtra a -> Cores ->  Int -> FilePath -> Bool -> WithExtra a -> Property
prop_specs_toolchain  _ cores time out newConjure WithExtra{..} = do
    let specE = getSpec inner_
        sp    = toSpec specE
    fst (typeChecks sp) ==>
        monadicIO $ do
            ts <- run timestamp >>= return . show
            -- num <- run (randomRIO (10,99) :: IO Int)  >>= return . show
            let num = show ts_int_
            let uname  =  (ts ++ "_" ++ num )
            let outdir =  (out </> uname)
            run $ createDirectoryIfMissing True outdir
            run $  writeFile (outdir </> "spec.logs" ) (renderNormal wlogs_)
            run $  writeFile (outdir </> "spec.specE" ) (show specE)


            result <- run $ runToolchain' run_seed_ cores sp (out </> uname ) time newConjure False
            classifyError uname result

    where
    errdir  = out </> "_errors"

    classifyError uname (Left SettingI{successful_=False,data_=RefineM ms  }) = do
        let inErrDir = errdir </> "zall" </> uname
        run $ createDirectoryIfMissing True inErrDir
        run $ renameDirectory (out </> uname ) inErrDir


        let

            f k CmdI{status_, kind_ } = do
                let mvDir = errdir </> (show kind_) </> (show status_) </> uname
                createDirectoryIfMissing True mvDir
                fps <- getDirectoryContents inErrDir
                let needed =  filter (allow k) fps

                forM needed $ \f -> do
                    copyFile (inErrDir </> f) (mvDir </> f)

                return mvDir

        run $ M.traverseWithKey f ms

        fail inErrDir


    classifyError uname (Right (_, SettingI{successful_=False,data_=SolveM ms })) = do

        let inErrDir = errdir </> "zall" </> uname
        run $ createDirectoryIfMissing True inErrDir
        run $ renameDirectory (out </> uname ) inErrDir

        let
            f k ResultI{last_status, erroed= Just index, results } = do
                let kind = kind_ (results !! index)
                let mvDir = errdir </> (show kind) </> (show last_status) </> uname
                createDirectoryIfMissing True mvDir

                fps <- getDirectoryContents inErrDir
                let needed =  filter (allow k) fps

                forM needed $ \f -> do
                    copyFile (inErrDir </> f) (mvDir </> f)

                return mvDir


            f _ _ = return ""

        run $ M.traverseWithKey f ms

        fail inErrDir

    classifyError _ _ =  return ()

allow :: String -> FilePath -> Bool
allow k f
    | k `isPrefixOf` f       = True
    | "json" `isSuffixOf` f  = True
    | "param" `isSuffixOf` f = True
    | "spec" `isPrefixOf` f  = True
    | "_" `isPrefixOf` f     = True
    | otherwise              = False

generateSpecs :: ArbSpec a => WithExtra a -> TArgs -> IO ()
generateSpecs unused r@TArgs{..} = do
    sss <- round `fmap` getPOSIXTime
    putStrLn . groom $ r
    putStrLn . show . vcat $ [ nn "Ωrseed_Start" (show rseed_), nn "ΩStartTime" (show sss) ]

    let maxSuccess = totalTime_  `div` perSpecTime_

    createDirectoryIfMissing  True tmpdir

    case rseed_ of
        Just i -> setStdGen (mkStdGen i)
        Nothing -> return ()

    rgenS <- createGen rseed_
    putStrLn . show . vcat $ [ nn "ΩrgenS_Start" (show rgenS) ]


    case typecheckOnly_ of
        (Just times) -> do
            putStrLn $ "Type checking " ++ (show times) ++
                " random specs, with depth up to size 5"

            (failed, ourErrors) <- typecheckHelper rgenS times [] [] 0
            saveFailures failed (baseDirectory_ </> "failures.txt")
            saveFailures' "gen.error" ourErrors (baseDirectory_ </> "genErrors.txt")

        Nothing -> do
            putStrLn "Generating specs, with depth up to size 5"
            startTime <- round `fmap` getPOSIXTime

            failed <- helper rgenS startTime []
            saveFailures failed (baseDirectory_ </> "failures.txt")

    where

    prop_run =
        if runToolchain_ then
            prop_specs_toolchain unused cores_ perSpecTime_ baseDirectory_ newConjure_
        else
            prop_specs_refine unused cores_ perSpecTime_ baseDirectory_ newConjure_


    tmpdir  = baseDirectory_ </> "temp"
    errdir  = baseDirectory_ </> "_errors"


    saveFailures :: [String] -> FilePath -> IO ()
    saveFailures  = saveFailures' "spec.essence"

    saveFailures' :: String -> [String] -> FilePath -> IO ()
    saveFailures' name failed fp  = do
        let paths = map (\fn -> baseDirectory_ </> fn </> name) failed
        print ("Current Failures found" :: String)
        putStrLn (unlines paths)
        writeToFile ( fp ) paths

    writeToFile name arr =
      do h <- openFile (name) WriteMode
         mapM_ (hPutStrLn h) arr
         hClose h

    createGen r =
        case r of
            Nothing    -> return Nothing
            Just iseed -> do
                let g = mkQCGen iseed
                putStrLn . show . vcat $ [ nn "Ωiseed" iseed,  nn "ΩcreateGen" (show g) ]

                return $ Just $ g

    splitGen :: Maybe QCGen -> (Maybe QCGen, Maybe QCGen)
    splitGen Nothing = (Nothing, Nothing)
    splitGen (Just rnd) = let (a,b) = split rnd in
        (Just a, Just b)

    helper :: Maybe QCGen -> Int -> [String] -> IO [String]
    helper rgenS startTime results = do
        let (next,rgen) = case rgenS of
                Just r -> (\(a,b) -> ((a + fromJust rseed_) `mod` (2^24) ,Just b)) $
                    randomR (0 :: Int ,(2 ^ 24 ))  r
                Nothing -> (0, Nothing)
        putStrLn . show . vcat $ [ "Ω-------" , nn "ΩrgenS" (show rgenS),  nn "Ωnext" next, nn "Ωrgen" (show rgen)]

        before <- round `fmap` getPOSIXTime

        let maxSuccess = (totalTime_ - (before - startTime)) `div` perSpecTime_

        case maxSuccess <= 0 of
            True  -> return results
            False -> do

                r <- quickCheckWithResult2 rgen stdArgs{QC.maxSize=size_,maxSuccess}
                    prop_run
                let results'  = addResults r results

                after <- round `fmap` getPOSIXTime
                putStrLn $ "Running for " ++ (show $ after - startTime ) ++ " s"

                case (r, after - startTime < totalTime_) of

                    (GaveUp{}, _ ) -> do
                        putStrLn "Gaveup"
                        return results'

                    (_, False) -> do
                        putStrLn "Time up"
                        return results'

                    (f@Failure{reason}, True)  -> do
                        saveFailures results' (tmpdir </> (show after) <.> "txt")
                        writeFile (baseDirectory_ </> reason </> "spec.qc")  ( show f{output=""})
                        nseed <- createGen $ (+next) <$> rseed_
                        helper nseed startTime results'
                        return results

                    (_, True) -> do
                        nseed <- createGen $ (+next) <$> rseed_
                        helper nseed startTime results'
                        return results

    addResults :: Result ->  [String] -> [String]
    addResults Failure{reason} arr = (reason) : arr
    addResults _ arr = arr


    typecheckHelper :: Maybe QCGen -> Int -> [String] -> [String] -> Int
        -> IO ([String], [String])
    typecheckHelper rgenS left results ourErrors index = do
        let (rgen, rgenN) =  splitGen rgenS

        putStrLn $ "left: " ++ (show left)

        let indexStr = padShowInt 4 index
            dir = baseDirectory_ </> (indexStr)
        createDirectoryIfMissing True dir

        r <- quickTypeCheck1 rgen unused
            (dir </> "spec.essence")
            stdArgs{QC.maxSize=size_,maxSuccess=left}
        case r of
            (Nothing) -> do
                return (results, ourErrors)
            (Just (count, output)) -> do

                specExist <-  doesFileExist (dir </> "spec.essence")
                when (not specExist) $ do
                    writeFile (dir </> "gen.error") output

                let (results', ourErrors') =
                        if specExist then
                            ( indexStr : results, ourErrors)
                        else
                            ( results, indexStr : ourErrors)

                let leftAfter = left - count
                if leftAfter > 0 then
                    typecheckHelper rgenN (leftAfter) results' ourErrors' (index+1)
                else
                    return (results', ourErrors')



quickTypeCheck1 :: (ArbSpec a) => Maybe QCGen -> WithExtra a -> FilePath -> Args ->  IO (Maybe (Int, String))
quickTypeCheck1 rgen unused fp args = do

    result <- quickCheckWithResult2 rgen args{chatty=False} (prop_typeCheckSave fp unused)
    case result of
        Failure {numTests,reason, output} -> do
            return (Just (numTests, output ))
        _ -> return Nothing

    where

prop_typeCheckSave :: (ArbSpec a) => FilePath -> WithExtra a -> WithExtra a -> Property
prop_typeCheckSave fp _  WithExtra{..} = do
    let specE = getSpec inner_
        sp = toSpec specE
        (res,doc) = typeChecks sp
    monadicIO $ do
        if res then
            return ()
        else do
            -- run $ print . pretty $ doc
            run $ print . pretty $ sp
            run $ writeSpec fp sp
            run $ writeFile (fp <.> "error") (renderWide (doc <+> vcat ["---", prettySpecDebug $ sp] ) )
            run $ writeFile (fp <.> "specE") (show specE ++ "\n\n" ++ groom specE)
            run $ writeFile (fp <.> "logs") (renderNormal wlogs_)


            fail (show doc)



takeFileName' :: FilePath -> FilePath
takeFileName' fp = case reverse fp of
    ('/': xs) -> takeFileName (reverse xs)
    _         -> takeFileName fp




prop_specs_type_check ::  ArbSpec a => a -> a -> Property
prop_specs_type_check _ arb = do
    let specE = getSpec arb
        sp = toSpec specE
        (res,doc) = typeChecks sp
    counterexample
        (show doc ++ (show $ pretty sp) )
        (res)

typeChecks :: Spec -> (Bool, Doc)
typeChecks sp = case fst $ runCompESingle "Error while type checking." $
    typeCheckSpec sp of
        Left  e  -> (False, e)
            -- trace (show e ++ (show . pretty $ sp)) False
        Right () -> (True, "")


rmain :: Int -> IO ()
rmain n =
    quickCheckWith stdArgs{QC.maxSize=n,maxSuccess=50}
    (prop_specs_refine (undefined :: WithExtra SpecE) 7 10  "__")

cmain :: ArbSpec a => WithExtra a -> Maybe Int -> Int -> IO ()
cmain unused seedInt n = do

    rgen  <- case seedInt of
            Just i  -> return $ Just $ mkQCGen i
            Nothing -> return Nothing


    c <- getCurrentTime
    let (y,m,d) = toGregorian $ utctDay c
    home <- getHomeDirectory
    date <- round `fmap` getPOSIXTime

    let dir = home </> "__" </> "logs" </> "cmain"
            </> ( intercalate "-" [show y, padShowInt 2 m, padShowInt 2 d] )
            </> (show date)

    createDirectoryIfMissing True dir

    res <- quickCheckWithResult2 rgen stdArgs{QC.maxSize=n,maxSuccess=3000}
        (prop_typeCheckSave (dir </> "spec.essence") unused  )
    case res of
        f@Failure{reason, output, usedSize} -> do
            writeFile (dir </> "spec.output")  (output ++ "\nseed: " ++ (show seedInt) ++ "\n" ++ show f{output=""})
            putStrLn $ "seed: " ++ (show seedInt)
            putStrLn $ "usedSize: " ++ (show usedSize)
        _ -> return ()
