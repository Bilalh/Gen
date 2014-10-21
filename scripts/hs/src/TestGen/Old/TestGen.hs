{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module TestGen.Old.TestGen where

import TestGen.Old.Create
import TestGen.Old.Data
import Common.Helpers
import TestGen.Runner

import Language.E
import Language.E.Pipeline.ReadIn(writeSpec)

import Control.Monad.Trans.State.Strict(execStateT)

import System.Directory(createDirectoryIfMissing)
import System.FilePath((</>), (<.>))
import System.Random(mkStdGen)

import Text.Groom(groom)


run :: (MonadIO m, MonadGG m) =>  StdGen -> Float -> m ()
run _ limit | limit <= 0  = do
    liftIO $ putStrLn $ "RUN NO TIME LEFT"

run seed limit = do
    liftIO $ putStrLn $ "RUN"
    liftIO $ putStrLn $ show limit ++ " seconds left"
    this <-get
    liftIO $ putStrLn . groom $ this

    nestl <- gets gMaxNesting
    (spec,st) <- runStateT chooseSpec defaultGenGlobal{
        eGen=seed
       ,eMaxNesting=nestl
       }

    liftIO $ print . pretty $ spec

    ts <- liftIO timestamp >>= return .show
    dir <- gets gBase >>= \d -> return $ d </> ts
    liftIO $ createDirectoryIfMissing True  dir

    let name = (dir </> ts <.> ".essence")
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

        doRes _ =  error "doRes"

        checkConsistent ( Right (_, s@SettingI{consistent_}) )
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


_main :: Float -> Int -> Int -> IO()
_main total perSpec seed = do
    let globalState = GenGlobal{
                       gBase = "__", gSeed = seed
                     , gTotalTime=total, gSpecTime=perSpec
                     , gErrorsRefine=[], gErrorsSolve=[],gInconsistent=[]
                     , gCount=0, gMaxNesting = 1}
    main' globalState


runRefine' :: Spec -> FilePath -> Int -> IO RefineR
runRefine' spec dir specTime = do
    print . pretty $ spec

    createDirectoryIfMissing True  dir

    let name = (dir </> "spec" <.> ".essence")
    writeSpec name spec

    let specLim = specTime
    result <- runRefine name dir specLim
    putStrLn . groom $  result
    return result
