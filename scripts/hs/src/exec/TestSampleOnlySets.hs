module Main where

import TestGen.Prelude

import TestGen.Arbitrary.Arbitrary(spec'', WithLogs)
import TestGen.Arbitrary.Type(atype_only)
import TestGen.Helpers.Args(parseArgs)
import TestGen.QC(generateSpecs)


main :: IO ()
main = do
    args <- parseArgs
    print args

    generateSpecs (undefined :: WithLogs SpecE) args
    putStrLn "<<Finished>>"

newtype S1 = S1 SpecE
    deriving Show  

instance ArbSpec S1 where
    getSpec (S1 sp) = sp
    wrapSpec sp  = S1 sp
    tyGens _ = def{
        gen_atype = atype_only [ TInt, TBool, TSet TAny  ]
        }
    
instance Arbitrary S1 where
    arbitrary = S1 . fst <$> spec'' 1 (tyGens (undefined :: S1))
         
