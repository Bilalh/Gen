--{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}

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
import Data.Maybe(fromJust)

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
    -- maxN <- gets eMaxNesting
    -- levels <-rangeRandomG (1,maxN)
    levels <- gets eMaxNesting
    dom :: EssenceDomain  <- pickVal levels

    i <- gets eFindIndex
    let name = T.pack $  "var" ++  (show  i)
    fs <- gets eFinds
    modify ( \s-> s{eFindIndex = i+1
                   ,eFinds = (name,  toEssence dom) : fs  }  )
    return ()

makeEs :: MonadGen m  => m [E]
makeEs = do
    varsNum <-  rangeRandomG (1,3)
    mapM_ (\_ -> chooseFindsDomain) [1..varsNum]
    gs <- gets eFinds
    return $  fmap (\(n,e) -> mkFind ((mkName n), e) ) gs

run :: (MonadIO m, MonadGG m) =>  StdGen -> Float -> m ()
run _ limit | limit <= 0  = do
    liftIO $ putStrLn $ "RUN NO TIME LEFT"

run seed limit = do
    liftIO $ putStrLn $ "RUN"
    liftIO $ putStrLn $ show limit ++ " seconds left"
    this <-get
    liftIO $ putStrLn . groom $ this

    nestl <- gets gMaxNesting
    (es,st) <- runStateT makeEs GenState{
        eFinds=[]
       ,eFindIndex=0
       ,eGen=seed
       ,eMaxNesting=nestl
       }

    ts <- liftIO timestamp >>= return .show
    dir <- gets gBase >>= \d -> return $ d </> ts
    liftIO $ createDirectoryIfMissing True  dir

    let name = (dir </> ts <.> ".essence")
    spec <- liftIO $ mkSpec es
    liftIO $ writeSpec name spec

    specLim <- gets gSpecTime
    result <- liftIO $ runToolChain name dir specLim
    liftIO $ putStrLn . groom $  result

    let
        doRes ( Right (r, s@SettingI{successful_}) )
            | successful_ = nextNesting nestl
            >> run (eGen  st) (limit - time_taken_ r - time_taken_ s)
        -- Refinement finished without errors, but no time left to solve
        doRes ( Left (SettingI{successful_,time_taken_}) )
            | successful_ = nextNesting nestl >> run (eGen st) (limit - time_taken_)

        doRes ( Left r@SettingI{successful_,time_taken_})
            | not successful_ = do
                storeRefineError r
                nextNesting nestl
                run (eGen  st) (limit - time_taken_)
        doRes ( Right (r, s@SettingI{successful_}) )
            | not successful_ = do
                storeSolveError s
                nextNesting nestl
                run (eGen  st) (limit - time_taken_ r - time_taken_ s )

        checkConsistent aa@( Right (_, s@SettingI{consistent_}) )
            | not consistent_ = do
                storeInconsistent s

        checkConsistent _ = return ()

    checkConsistent result
    doRes result


    where

    storeRefineError :: MonadGG m => RefineR -> m ()
    storeRefineError r = do
       n <- gets gErrorsRefine
       modify (\st -> st{gErrorsRefine = r:n})

    storeSolveError :: MonadGG m => SolveR -> m ()
    storeSolveError s = do
       n <- gets gErrorsSolve
       modify (\st -> st{gErrorsSolve = s:n})

    storeInconsistent :: MonadGG m => SolveR -> m ()
    storeInconsistent s = do
       n <- gets gInconsistent
       modify (\st -> st{gInconsistent = s:n})

    nextNesting :: MonadGG m => Int -> m ()
    nextNesting level = do
        GenGlobal{gMaxNesting, gCount} <- get

        case (level == gMaxNesting, gCount, level>5) of
            (False ,_,_)    -> setCount 0
            (True ,10,True) -> setNesting (5) >> setCount 0
            (True ,10,_)    -> setNesting (level + 1) >> setCount 0
            (True ,i,_)     -> setCount (i+1)


    setCount :: MonadGG m => Int ->  m ()
    setCount i = do
        modify (\st -> st {gCount = i })

    setNesting :: MonadGG m => Int ->  m ()
    setNesting i = do
        modify (\st -> st {gMaxNesting = i })

-- classifyStatus :: MonadGG m => ResultI -> m ()
-- classifyStatus r@ResultI{..} = case last_status of
--         Success_ -> return ()
--         Timeout_ -> do
--             n <- gets gErrors_timeout
--             modify ( \st -> st{ gErrors_timeout=r:n  })
--         NumberToLarge_ -> do
--             n <- gets gErrors_no_use
--             modify ( \st -> st{ gErrors_no_use =r:n  })
--         _ -> do
--             n <- gets gErrors
--             modify ( \st -> st{ gErrors=r:n  })

main :: IO ()
main = do
    globalState <- parseArgs
    main' globalState
    putStrLn "<<Finished>>"

maing :: Float -> Int -> Int -> IO()
maing total perSpec seed = do
    let globalState = GenGlobal{
                       gBase = "__", gSeed = seed
                     , gTotalTime=total, gSpecTime=perSpec
                     , gErrorsRefine=[], gErrorsSolve=[],gInconsistent=[]
                     , gCount=0, gMaxNesting = 2}
    main' globalState

main' :: GenGlobal -> IO ()
main' gs = do
    print gs
    -- seedd :: Int <- randomIO
    -- let seed = mkStdGen seedd
    let seed = mkStdGen (gSeed gs)

    finalState <- execStateT (run seed (gTotalTime gs) ) gs
    putStrLn . groom $ finalState
    saveAsJSON finalState (gBase finalState </> "state.json")
    return ()

ll :: E -> EssenceLiteral
ll = fromJust . toEssenceLiteral

_m :: StateT GenState IO a -> IO (a, GenState)
_m f = do
    seedd :: Int <- randomIO
    let seed = mkStdGen seedd
    runStateT f GenState{
            eFinds=[]
           ,eFindIndex=0
           ,eGen=seed
           ,eMaxNesting=2
           }


sr :: MonadGen m => m [Integer]
sr = do
    let lst = [1,2,3,4,5,6] :: [Integer]
    sample lst 2

mkSpec :: [E] -> IO Spec
mkSpec es = do

    let spec = Spec (LanguageVersion "Essence" [1,3])
         . listAsStatement
         -- . normaliseSolutionEs
         $ es
    print .  pretty $ spec
    return spec

