{-# LANGUAGE FlexibleInstances, QuasiQuotes, UndecidableInstances #-}
module Gen.WeightsTest ( tests ) where

import Gen.Essence.St
import Gen.Essence.Type ()
import Gen.Essence.Constant ()
import Gen.Essence.Expr ()
import Gen.Helpers.SizeOf
import Gen.Imports
import Gen.Reduce.Simpler
import Gen.TestPrelude
import Gen.Helpers.TypeOf
import qualified Gen.Essence.Weightings  as Weights

tests :: TestTree
tests = testGroup "GenerateOnly"
  [
   testGroup "QC"
   [
     qc_tests True  "Type" (Proxy :: Proxy Type)
   , qc_tests True  "Constant" (Proxy :: Proxy Constant)
   , qc_tests True  "Expr" (Proxy :: Proxy (Expr))
   , qc_tests True  "Domain Expr" (Proxy :: Proxy (Domain () Expr))
   ]

  ]



data Limited a =  Limited Int (String,KeyMap) GenerateConstraint a
    deriving (Eq)

instance (Pretty a, Show a, DepthOf a) => Pretty (Limited a) where pretty = pretty . show
instance (Pretty a, Show a, DepthOf a) => Show (Limited a)   where
 show (Limited d ks con _) = renderSized 100 $ hang "Limited" 4 $ vcat
          [ nn "GivenDepth:"  (d)
          , nn "Con       :"  (con)
          , nn "Con groom :"  (groom con)
          , nn "Weights   :"  (ks)
          ]

instance (Generate a, Simpler a a, NeedsType a ) => Arbitrary (Limited a) where
  arbitrary = sized $ \s -> do
    i <- choose (2,  (max 1 (min s 3)) )
    nTypes <- choose (1,3)
    choices@(_,ks) <- elements $ Weights.byType nTypes
    ty <- runGenerateNullLogs GNone def{depth=i,weighting=ks }
    let con = GType ty
    v <- runGenerateNullLogs con def{depth=i,weighting=ks }
    return $ Limited i choices con v



qc_tests :: forall p
          . (Generate p, Simpler p p, DepthOf p, TTypeOf p, NeedsType p)
         => Bool -> String -> Proxy p  -> TestTree
qc_tests _ title _ =
  testGroup title $
   catMaybes $ [
     Just $ testProperty "Weights Can Generate" $
       \(Limited _ _ _ (a :: p) ) ->  a == a
   ]
