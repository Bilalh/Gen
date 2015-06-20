module Gen.Essence.Rnd where

import Gen.Essence.St
import Gen.Imports
import System.Random               (Random)
import Test.QuickCheck             (choose,elements,choose)


frequency3 :: [(Int, GenSt a)] -> GenSt a
frequency3 [] = error "frequency3 used with empty list"
frequency3 xs0 = do
  freqError $line "frequency3" xs0
  choose3 (1, tot) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = nnError "frequency3.pick used with empty list" []


elemFreq3 :: [(Int, a)] -> GenSt a
elemFreq3 [] = error "frequency3 used with empty list"
elemFreq3 xs0 =  choose3 (1, tot) >>= (`pick` (map (\(a,b) -> (a,pure b) ) xs0))
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = nnError "frequency3.pick used with empty list" []


choose3 :: Random a => (a,a) -> GenSt a
choose3 rng = lift . lift $ choose rng

oneof3 :: [GenSt a] -> GenSt a
oneof3 [] = nnError "oneof3 used with empty list" []
oneof3 gs = choose3 (0,length gs - 1) >>=   (gs `at`)

elements3 :: [a] -> GenSt a
elements3 as  = lift . lift $ elements as

vectorOf3 :: Int -> GenSt a -> GenSt [a]
vectorOf3 k gen = sequence [ gen | _ <- [1..k] ]

-- | Generates a random length between the specifed bounds.
bounded3 :: (Int,Int) -> GenSt a -> GenSt [a]
bounded3 (l,u) gen = do
    k <- choose3 ( l, u )
    vectorOf3 k gen


freqError :: (MonadState St m) => String -> Doc -> [(Int, a)] -> m ()
freqError l dc defs = do
    when (all ( (<=0) . fst) defs ) $ nnError "freqError" $
         pretty l : pretty  dc : map (pretty . fst) defs
