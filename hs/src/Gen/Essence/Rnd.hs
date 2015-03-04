module Gen.Essence.Rnd where

-- import Gen.Prelude
-- import Gen.Reduce.Data
import System.Random.TF(newTFGen,TFGen)
-- import Gen.AST.Imports

import Conjure.Prelude
-- import Conjure.Language.Definition
-- import Conjure.Language.Domain

import System.Random
  ( Random
  , randomR
  , split
  )

newtype Rnd a = MkRnd{
  unGen :: TFGen -> Int -> a
  }

instance Functor Rnd where
  fmap f (MkRnd h) =
    MkRnd (\r n -> f (h r n))

instance Applicative Rnd where
  pure  = return
  (<*>) = ap

instance Monad Rnd where
  return x =
    MkRnd (\_ _ -> x)

  MkRnd m >>= k =
    MkRnd (\r n ->
      let (r1,r2)  = split r
          MkRnd m' = k (m r1 n)
       in m' r2 n
    )


generate :: Rnd a -> IO a
generate (MkRnd g) =
  do r <- newTFGen
     return (g r 31)

choose :: Random a => (a,a) -> Rnd a
choose rng = MkRnd (\r _ -> let (x,_) = randomR rng r in x)

aa :: Rnd Int
aa = choose (1,10)

oneof :: [Rnd a] -> Rnd a
oneof [] = error "Rnd.oneof used with empty list"
oneof gs = choose (0,length gs - 1) >>= (gs `at`)
