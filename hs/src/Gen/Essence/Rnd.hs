module Gen.Essence.Rnd where

import Gen.Essence.St
import Gen.Imports
import System.Random               (Random)
import Test.QuickCheck             (choose,elements,choose)


frequency3 :: [(Int, GenSt a)] -> GenSt a
frequency3 [] = error "frequency3 used with empty list"
frequency3 xs0 =  choose3 (1, tot) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error "frequency3.pick used with empty list"

choose3 :: Random a => (a,a) -> GenSt a
choose3 rng = lift . lift $ choose rng

oneof3 :: [GenSt a] -> GenSt a
oneof3 [] = error "oneof3 used with empty list"
oneof3 gs = choose3 (0,length gs - 1) >>=   (gs `at`)

elements3 :: [a] -> GenSt a
elements3 as  = lift . lift $ elements as

vectorOf3 :: Int -> GenSt a -> GenSt [a]
vectorOf3 k gen = sequence [ gen | _ <- [1..k] ]
