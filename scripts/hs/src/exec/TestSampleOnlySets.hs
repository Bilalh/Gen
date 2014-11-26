module Main where

import TestGen.Arbitrary.Arbitrary
import TestGen.Helpers.Args(parseArgs)
import TestGen.Prelude
import TestGen.QC(generateSpecs)


main :: IO ()
main = do
    args <- parseArgs
    print args

    generateSpecs (undefined :: SpecE) args
    putStrLn "<<Finished>>"

newtype S1 = S1 SpecE
    deriving Show  

instance ArbSpec S1 where
    getSpec (S1 sp) = sp
    tyGens _ = def 
    
instance Arbitrary S1 where
    arbitrary = S1 . fst <$> spec'' 1 (tyGens (undefined :: S1))
         
