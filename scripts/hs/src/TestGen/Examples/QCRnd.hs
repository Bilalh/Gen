{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
module QCRnd where

import Common.Helpers
import TestGen.ToEssence
import Language.E hiding(trace)
import Debug.Trace(trace)

import Test.QuickCheck
import Control.Monad
import TestGen.EssenceConstraints

import qualified Data.Text as T
import System.IO

class Variant a where
  valid   :: Gen a
  invalid :: Gen a

proper1 f = liftM  f valid
proper2 f = liftM2 f valid valid
proper3 f = liftM3 f valid valid valid
proper4 f = liftM4 f valid valid valid valid
proper5 f = liftM5 f valid valid valid valid valid

bad1 f = liftM f invalid
bad2 f = oneof $ tail [ liftM2 f g1 g2 | g1<-[valid, invalid], g2<-[valid, invalid] ]
bad3 f = oneof $ tail [ liftM3 f g1 g2 g3 | g1<-[valid, invalid], g2<-[valid, invalid], g3<-[valid, invalid] ]
bad4 f = oneof $ tail [ liftM4 f g1 g2 g3 g4 | g1<-[valid, invalid], g2<-[valid, invalid], g3<-[valid, invalid], g4<-[valid, invalid] ]
bad5 f = oneof $ tail [ liftM5 f g1 g2 g3 g4 g5 | g1<-[valid, invalid], g2<-[valid, invalid], g3<-[valid, invalid], g4<-[valid, invalid], g5<-[valid, invalid] ]


instance Variant a => Arbitrary a where
  arbitrary   = oneof [valid, invalid]

data Record     = InputRecord Name Number
             | OutputRecord Name Number OutputType deriving Show
data Number     = Number String                       deriving Show
data Name       = Name String                         deriving Show
data OutputType = OutputType String                   deriving Show


-- For definition of `neStringOf` see below, for now it is sufficient
-- to say that `neStringOf first next` produces non-empty string whose
-- first character is taken from `first` and all subsequent - from
-- `next`
garbledString = neStringOf ".,_+-" "abc0!@#$%^&*()."

instance Variant Number where
  valid   = liftM Number $ resize 4 $ neStringOf "123456789" "0123456789"
  invalid = liftM Number $ resize 4 $ garbledString
instance Variant Name where
  valid   = liftM Name $ elements [ "foo", "bar", "baz" ]
  invalid = liftM Name garbledString
instance Variant OutputType where
  valid   = liftM OutputType $ elements [ "Binary", "Ascii" ]
  invalid = liftM OutputType garbledString

instance Variant Record where
  valid   = oneof [ proper2 InputRecord
                  , proper3 OutputRecord ]
  invalid = oneof [ bad2 InputRecord
                  , bad3 OutputRecord ]


neStringOf chars_start chars_rest =
  do s <- elements chars_start
     r <- listOf' $ elements chars_rest
     return (s:r)

listOf' :: Gen a -> Gen [a]
listOf' gen = sized $ \n ->
  do k <- choose (0,n)
     vectorOf' k gen

vectorOf' :: Int -> Gen a -> Gen [a]
vectorOf' k gen = sequence [ gen | _ <- [1..k] ]


data DataDefinition = DataDefinition Name Record deriving Show

aaa =
  do let num = 200       -- Number of test cases in each dataset.
     let config =        -- Describe several test datasets for "DataDefinition"
                         -- by defining how we want each component of DataDefinition
                         -- for each particular dataset - valid, invalid or random
           [ ("All_Valid",       "txt",  num, (valid,     valid    ))
           , ("Invalid_Name",    "txt",  num, (invalid,   valid    ))
           , ("Invalid_Record" , "txt" , num, (valid,     invalid  ))
           , ("Random",          "txt",  num, (arbitrary, arbitrary))
           ]
     mapM_ create_test_set config

create_test_set (fname, ext, count, gens) =
  do test_set <- sample' $ vectorOf count (mkDataDef gens)
     zipWithM_ (writeToFile fname ext) [1..] test_set
  where
  mkDataDef (gen_name, gen_rec) = liftM2 DataDefinition gen_name gen_rec

writeToFile name_prefix suffix n x =
  do h <- openFile (name_prefix ++ "_" ++ pad n ++ "." ++ suffix) WriteMode
     hPutStrLn h $ show x
     hClose h
  where pad n = reverse $ take 4 $ (reverse $ show n) ++ (repeat '0')

