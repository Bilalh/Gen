{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module TestGen.QC.ArbitrarySpec where

import Language.E hiding(trace)

import TestGen.EssenceDomain(EssenceDomain(..))
import TestGen.EssenceConstraints
import TestGen.ToEssence(toEssence)
import Common.Helpers(mkFind,mkName,mkSpec)

import Test.QuickCheck
import Control.Monad(liftM2)
import qualified Data.Text as T

data SpecE = SpecE [(Text, EssenceDomain)] [Eexpr]
    deriving(Show)


instance Arbitrary SpecE where
    arbitrary = arbitrarySpec


arbitrarySpec :: Gen SpecE
arbitrarySpec = do
    doms <- listOf1 arbitraryDom
    let withNames =  zipWith (\d i -> (name i , d)) doms [1 :: Int ..]
    exprs <- listOf arbitraryExpr

    return $ SpecE withNames ( Elit (ELB True) :  exprs)

    where name i =  T.pack $  "var" ++  (show  i)



-- arbitraryDoms :: Gen [(Text, EssenceDomain)]
-- arbitraryDoms = sized $ \n -> do
--     k <- choose (1, n `min` 1 )

--     doms <- sequence [ arbitraryDom | i <- [1..k] ]
--     -- let withNames =  zipWith (\d i -> (name i , d)) doms [1..]
--     _2

--     where name i =  T.pack $  "var" ++  (show  i)

arbitraryDom ::  Gen (EssenceDomain)
arbitraryDom = elements [ DInt 1 5 ]

arbitraryExpr :: Gen Eexpr
arbitraryExpr = elements [ Elit (ELB True), Elit (ELB False)  ]


instance Arbitrary Eexpr where
    arbitrary = sized arbitrarySized

class Arbitrary a => ArbitrarySized a where
    arbitrarySized :: Int ->  Gen a
    arbitrarySized _ = error "no default sized generator"

instance Arbitrary a => ArbitrarySized [a] where
  arbitrarySized n  = do
       k <- choose (0,n)
       sequence [ arbitrary | _ <- [1..k] ]


instance ArbitrarySized Eexpr where
    arbitrarySized 0 = oneof [
         liftM Evar (arbitrary)
        ,liftM Elit (arbitrarySized 0)
        ]
    arbitrarySized n = oneof [
            liftM  Evar (arbitrary)
           ,liftM2 Egt  (arbitrarySized ((n-1) `div` 2)) (arbitrarySized ((n-1) `div` 2))
           ,liftM2 Eneq (arbitrarySized ((n-1) `div` 2)) (arbitrarySized ((n-1) `div` 2))
           ,liftM  Elit (arbitrarySized (n-1))
        ]

instance Arbitrary EssenceLiteral where
    arbitrary = sized arbitrarySized

instance ArbitrarySized EssenceLiteral where
    arbitrarySized 0 = oneof [
         liftM ELB arbitrary
        ,liftM ELI arbitrary
        ]
    arbitrarySized n = oneof [
         liftM ELB arbitrary
        ,liftM ELI arbitrary
        ,liftM ELSet arbitrary
        ]

instance Arbitrary Text where
    arbitrary = liftM (T.pack . ("var_" ++) .  show) $ choose (10,99 :: Integer)


toSpec :: SpecE -> Spec
toSpec (SpecE doms cons) =
    let constraints = [xMake| topLevel.suchThat := map toEssence cons |]
        finds = fmap (\(n,e) -> mkFind ((mkName n), toEssence e) ) doms
    in
        mkSpec $ finds ++ [constraints]
