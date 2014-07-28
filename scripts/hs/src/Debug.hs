{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Debug where

import Data
import Language.E

import Control.Monad.Trans.State.Strict(StateT)
import Data.Maybe(fromJust)
import System.Random(randomIO,mkStdGen)

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

_tm :: (MonadGen m, MonadIO m) => m [[Int]]
_tm = do
    a <- mapM ff [1..5 :: Int]
    return a

    where
    ff :: (MonadGen m, MonadIO m) => Int ->  m [Int]
    ff i | i > 50 = return [0 ]
    ff i = do

        gen1 <- gets eGen
        liftIO $ putStrLn $ "Before " ++ show i  ++ " " ++ show gen1
        r <- rangeRandomG (1,100)
        gen2 <- gets eGen
        liftIO $ putStrLn $ "After  " ++ show i ++ " " ++ show gen2 ++ " r:" ++ show r

        a <- ff r
        return $ r : a

myMap :: (MonadGen m, MonadIO m) => (a -> m b) -> [a] -> m [b]
myMap _ [] = return []

myMap f [a] = do
    b <- f a
    return [b]

myMap f (a:aas) = do
    b <- f a
    bs <- myMap f aas
    return $ b : bs


sr :: MonadGen m => m [Integer]
sr = do
    let lst = [1,2,3,4,5,6] :: [Integer]
    sample lst 2


