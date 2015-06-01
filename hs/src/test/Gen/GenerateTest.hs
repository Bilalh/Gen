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
import Gen.Helpers.TypeOf

tests :: TestTree
tests = testGroup "GenerateOnly"
  [
   testGroup "QC"
   [
     -- qc_tests True "Type" (Proxy :: Proxy Type)  -- Too much hassle to not not give a type
     qc_tests False "Constant" (Proxy :: Proxy Constant)
   , qc_tests False "AbstractLiteral Constant" (Proxy :: Proxy (AbstractLiteral Constant))
   , qc_tests True "AbstractLiteral Expr" (Proxy :: Proxy (AbstractLiteral Expr))
   , qc_tests True "Expr" (Proxy :: Proxy (Expr))
   ]

  ]


data Limited a =  Limited a
    deriving (Eq)

instance (Pretty a, Show a) => Pretty (Limited a) where pretty = pretty . show
instance (Pretty a, Show a) => Show (Limited a)   where
 show (Limited a) = renderSized 100 $ hang "Limited" 4 $ vcat
          -- [ nn "Groomed :" (groom a)
          [ nn "Pretty  :"  (a)
          ]

instance (Generate a, Simpler a a) => Arbitrary (Limited a) where
  arbitrary = sized $ \s -> do
    i <- choose (1,  (max 0 (min s 3)) )
    ty :: Type <- runGenerateNullLogs GNone def{depth=i}
    Limited <$> runGenerateNullLogs (GType ty) def{depth=i}




qc_tests :: forall p
          . (Generate p, Simpler p p, DepthOf p, TTypeOf p)
         => Bool -> String -> Proxy p  -> TestTree
qc_tests b title _ =
  testGroup title $
   catMaybes $ [
     Just $ testProperty "Can Generate" $
       \(Limited (a :: p)) ->  a == a
   , j $ testProperty "TypeOf" $
       \(Limited (a :: p)) -> do
         let ty = runIdentity $ ttypeOf a
         ty == ty
   ]
   where j f = if b then
                   Just f
               else
                   Nothing
