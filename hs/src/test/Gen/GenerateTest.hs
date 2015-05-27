{-# LANGUAGE FlexibleInstances, QuasiQuotes, UndecidableInstances #-}
module Gen.GenerateTest ( tests ) where

import Gen.Essence.St
import Gen.Essence.Type ()
import Gen.Essence.Constant ()
import Gen.Essence.Expr ()
import Gen.Helpers.SizeOf
import Gen.Imports
import Gen.Reduce.Simpler
import Gen.TestPrelude
import Test.Tasty.QuickCheck    as QC
import Gen.Helpers.TypeOf

tests :: TestTree
tests = testGroup "GenerateOnly"
  [
   testGroup "QC"
   [
     qc_tests "Type" (Proxy :: Proxy Type)
   , qc_tests "Constant" (Proxy :: Proxy Constant)
   , qc_tests "AbstractLiteral Constant" (Proxy :: Proxy (AbstractLiteral Constant))
   , qc_tests "AbstractLiteral Expr" (Proxy :: Proxy (AbstractLiteral Expr))
   , qc_tests "Expr" (Proxy :: Proxy (Expr))
   ]

  ]


data Limited a =  Limited a
    deriving (Eq)

instance (Pretty a, Show a) => Pretty (Limited a) where pretty = pretty . show
instance (Pretty a, Show a) => Show (Limited a)   where
 show (Limited a) = renderSized 100 $ hang "Limited" 4 $ vcat
          [ nn "Groomed :" (groom a)
          , nn "Pretty  :"  (a)
          ]

instance (Generate a, Simpler a a) => Arbitrary (Limited a) where
  arbitrary = sized $ \s -> do
    i <- choose (1,  (max 0 (min s 3)) )
    Limited <$> runGenerateNullLogs GNone def{depth=i}


qc_tests :: forall p
          . (Generate p, Simpler p p, DepthOf p, TTypeOf p)
         => String -> Proxy p  -> TestTree
qc_tests title _ =
  testGroup title $
   catMaybes $ [
     Just $ QC.testProperty "Is equal to self" $
       \(Limited (a :: p)) ->  a == a
   , Just $ QC.testProperty "TypeOf" $
       \(Limited (a :: p)) -> do
         let ty = runIdentity $ ttypeOf a
         ty == ty
   ]
