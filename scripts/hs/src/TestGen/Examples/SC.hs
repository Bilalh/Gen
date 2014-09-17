{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}

module TestGen.Examples.QC where

import Common.Helpers
import TestGen.ToEssence
import TestGen.EssenceConstraints
import Language.E

import Test.SmallCheck
import qualified Test.SmallCheck as SC
import Test.SmallCheck.Series

import Control.Monad(liftM2)

isPrime :: Integer -> Bool
isPrime _ = True

main =
  smallCheck 5 {- 5 is the testing depth -} $
    \(Positive n) ->
      (exists $ \(Positive d) -> d /= 1 && d /= n && n `mod` d == 0) ==> not (isPrime n)


a = smallCheck 1 $ f

    where
    f (Eneq _ _) = (exists $ _ )
