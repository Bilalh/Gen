--{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Helpers
import Test
import Data
import Runner
import Args(parseArgs)

import Language.E
import Language.E.Pipeline.ReadIn(writeSpec)
import Language.E.DomainOf
import Control.Monad.Trans.State.Strict(StateT,execStateT)

import Data.Set(Set)

import System.Directory(createDirectoryIfMissing)
import System.FilePath((</>), (<.>))
import System.Process(rawSystem)
import System.Random(randomIO,mkStdGen)

import Text.Groom(groom)

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Test.QuickCheck as Q

chooseFindsDomain :: MonadGen m => m ()
chooseFindsDomain = do
    levels <- rangeRandomG (1,2)
    dom :: EssenceDomain  <- pickVal (levels)

    i <- gets gFindIndex
    let name = T.pack $  "var" ++  (show  i)
    fs <- gets gFinds
    modify ( \s-> s{gFindIndex = i+1
                   ,gFinds = (name,  fromEssenceDomain dom) : fs  }  )
    return ()

makeEs :: MonadGen m  => m [E]
makeEs = do
    varsNum <- rangeRandomG (1,3)
    mapM_ (\_ -> chooseFindsDomain) [1..varsNum]
    gs <- gets gFinds
    return $  fmap (\(n,e) -> mkFind ((mkName n), e) ) gs

run :: (MonadIO m, MonadGG m) =>  StdGen -> Float -> m ()
run _ limit | limit <= 0  = return ()
run seed limit = do
    liftIO $ putStrLn $ show limit ++ " seconds left"
    (es,st) <- runStateT makeEs GenState{
        gFinds=[], gFindIndex=0, genSeed=seed}

    ts <- liftIO timestamp >>= return .show
    dir <- gets gBase >>= \d -> return $ d </> ts
    liftIO $ createDirectoryIfMissing True  dir

    let name = (dir </> ts <.> ".essence")
    spec <- liftIO $ mkSpec es
    liftIO $ writeSpec name spec

    specLim <- gets gSpecTime
    result <- liftIO $ runToolChain name dir specLim
    liftIO $ putStrLn . groom $  result

    _ <- classifyStatus result

    run (genSeed st) (limit - total_cpu_time result)

classifyStatus :: MonadGG m => ResultI -> m ()
classifyStatus r@ResultI{..} = case last_status of
        Success_ -> return ()
        Timeout_ -> do
            n <- gets gErrors_timeout
            modify ( \st -> st{ gErrors_timeout=r:n  })
        NumberToLarge_ -> do
            n <- gets gErrors_no_use
            modify ( \st -> st{ gErrors_timeout=r:n  })
        _ -> do
            n <- gets gErrors
            modify ( \st -> st{ gErrors=r:n  })

main :: IO ()
main = do
    globalState <- parseArgs
    main' globalState

mainG :: IO()
mainG = do
    seedd :: Int <- randomIO
    let globalState = GenGlobal{
                       gBase = "__", gSeed = seedd
                     , gTotalTime=5, gSpecTime=2
                     , gErrors=[],gErrors_no_use=[],gErrors_timeout=[]}
    main' globalState

main' :: GenGlobal -> IO ()
main' gs = do
    print gs
    -- seedd :: Int <- randomIO
    -- let seed = mkStdGen seedd
    let seed = mkStdGen (gSeed gs)

    finalState <- execStateT (run seed (gTotalTime gs) ) gs
    putStrLn . groom $ finalState
    saveAsJSON finalState (gBase finalState </> "results.json")
    return ()

mkSpec :: [E] -> IO Spec
mkSpec es = do

    let spec = Spec (LanguageVersion "Essence" [1,3])
         . listAsStatement
         -- . normaliseSolutionEs
         $ es
    print .  pretty $ spec
    return spec

