-- Basily some Test.QuickCheck functions to work with our stateT.
-- Plus some functions that use them
module TestGen.Helpers.QuickCheck2 where

import TestGen.Helpers.StandardImports


import TestGen.Arbitrary.Data(GG)

import Test.QuickCheck.Gen

import System.Random( Random )

import Test.QuickCheck
import Test.QuickCheck.Test
import Test.QuickCheck.Text
import Test.QuickCheck.Random
import Test.QuickCheck.State(State(..))
import Test.QuickCheck.Property(unProperty)

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
oneof2 gs = choose2 (0,length gs - 1) >>=   (gs `at`)

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
    k <- choose2 ( 0 `max` l, (u `min` n) `max` l )
    vectorOf2 k gen


-- | Generates one of the given values. The input list must be non-empty.
elements2 :: [a] -> GG a
elements2 as  = lift $ elements as


-- Test.QuickCheck.Test.quickCheckWithResult  allowing the seed to be changed
-- without specifying the size
quickCheckWithResult2 :: Testable prop => Maybe QCGen -> Args -> prop -> IO Result
quickCheckWithResult2 rgen a p = (if chatty a then withStdioTerminal else withNullTerminal) $ \tm -> do
     rnd <- case (replay a,rgen) of
              (Just (rnd,_),_)  -> return rnd
              (_, Just rnd)     -> return rnd
              _                 -> newQCGen

     test MkState{ terminal                  = tm
                 , maxSuccessTests           = maxSuccess a
                 , maxDiscardedTests         = maxDiscardRatio a * maxSuccess a
                 , computeSize               = case replay a of
                                                 Nothing    -> computeSize'
                                                 Just (_,s) -> computeSize' `at0` s
                 , numSuccessTests           = 0
                 , numDiscardedTests         = 0
                 , numRecentlyDiscardedTests = 0
                 , collected                 = []
                 , expectedFailure           = False
                 , randomSeed                = rnd
                 , numSuccessShrinks         = 0
                 , numTryShrinks             = 0
                 , numTotTryShrinks          = 0
                 } (unGen (unProperty (property' p)))
  where computeSize' n d
          -- e.g. with maxSuccess = 250, maxSize = 100, goes like this:
          -- 0, 1, 2, ..., 99, 0, 1, 2, ..., 99, 0, 2, 4, ..., 98.
          | n `roundTo` maxSize a + maxSize a <= maxSuccess a ||
            n >= maxSuccess a ||
            maxSuccess a `mod` maxSize a == 0 = (n `mod` maxSize a + d `div` 10) `min` maxSize a
          | otherwise =
            ((n `mod` maxSize a) * maxSize a `div` (maxSuccess a `mod` maxSize a) + d `div` 10) `min` maxSize a
        n `roundTo` m = (n `div` m) * m
        at0 _f s 0 0 = s
        at0 f _s n d = f n d
        property' q
          | exhaustive q = once (property q)
          | otherwise = property q
