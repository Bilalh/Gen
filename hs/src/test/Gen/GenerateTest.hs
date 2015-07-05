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
     qc_tests True  "Type" (Proxy :: Proxy Type)
   , qc_tests True  "Constant" (Proxy :: Proxy Constant)
   , qc_tests True  "AbstractLiteral Constant" (Proxy :: Proxy (AbstractLiteral Constant))
   , qc_tests True  "Domain Expr" (Proxy :: Proxy (Domain () Expr))
   , qc_tests False "AbstractLiteral Expr" (Proxy :: Proxy (AbstractLiteral Expr))
   , qc_tests True  "Expr" (Proxy :: Proxy (Expr))
   ]

  ]



data Limited a =  Limited a Int
    deriving (Eq)

instance (Pretty a, Show a, DepthOf a) => Pretty (Limited a) where pretty = pretty . show
instance (Pretty a, Show a, DepthOf a) => Show (Limited a)   where
 show (Limited a d) = renderSized 100 $ hang "Limited" 4 $ vcat
          [ nn "Pretty    :"  (a)
          , nn "GivenDepth:"  (d)
          , nn "RealDepth :"  (depthOf a)
          , nn "Groomed   :"   (groom a)
          ]

instance (Generate a, Simpler a a, NeedsType a ) => Arbitrary (Limited a) where
  arbitrary = sized $ \s -> do
    i <- choose (1,  (max 1 (min s 3)) )
    con <- giveConstraint (Proxy :: Proxy a) i
    Limited <$> runGenerateNullLogs con def{depth=i} <*> pure i


qc_tests :: forall p
          . (Generate p, Simpler p p, DepthOf p, TTypeOf p, NeedsType p)
         => Bool -> String -> Proxy p  -> TestTree
qc_tests b title _ =
  testGroup title $
   catMaybes $ [
     Just $ testProperty "Can Generate the" $
       \(Limited (a :: p) _) ->  a == a
   , j $ testProperty "TypeOf" $
       \(Limited (a :: p) _) -> do
         let ty = runIdentity $ ttypeOf a
         ty == ty
   ,  Just $ testProperty "Depth is consistent" $
       \(Limited (a :: p) i ) ->
           depthOf a <= fromIntegral i
   ]
   where j f = if b then
                   Just f
               else
                   Nothing
