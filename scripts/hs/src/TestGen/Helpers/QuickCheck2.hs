-- Basily some Test.QuickCheck functions to work with our stateT.
-- Plus some functions that use them
module TestGen.Helpers.QuickCheck2 where

import TestGen.Arbitrary.Data(GG)

import Test.QuickCheck.Gen

import Control.Monad.State.Strict(runStateT, MonadState(get, put))
import Control.Monad.Trans.Class as X ( MonadTrans(lift) )
import System.Random( Random )


-- | Generates a list of the given length.
vectorOf2 :: Int -> GG a -> GG [a]
vectorOf2 k gen = sequence [ gen | _ <- [1..k] ]

-- | Generates a random element in the given inclusive range.
choose2 :: Random a => (a,a) -> GG a
choose2 rng = lift $  choose rng

-- | Randomly uses one of the given generators. The input list
-- must be non-empty.
oneof2 :: [GG a] -> GG a
oneof2 [] = error "QuickCheck2.oneof used with empty list"
oneof2 gs = choose2 (0,length gs - 1) >>=   (gs !!)

-- | Chooses one of the given generators, with a weighted random distribution.
-- The input list must be non-empty.
frequency2 :: [(Int, GG a)] -> GG a
frequency2 [] = error "QuickCheck2.frequency used with empty list"
frequency2 xs0 = choose2 (1, tot) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error "QuickCheck2.pick used with empty list"


-- | Used to construct generators that depend on the size parameter.
sized2 :: (Int -> GG a) -> GG a
sized2 f = do
    st <- get
    (gen,st') <- lift . sized $ (flip runStateT st) . f
    put st'
    return $ gen


-- | Generates a random length between the specifed bounds.
--   The length depends on the size parameter.
listOfBounds :: (Int,Int) -> GG a -> GG [a]
listOfBounds (l,u) gen = sized2 $ \n -> do
    k <- choose2 ( 0 `max` l, u `min` n)
    vectorOf2 k gen

-- | Generates one of the given values. The input list must be non-empty.
elements2 :: [a] -> GG a
elements2 as  = lift $ elements as
