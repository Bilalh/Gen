{-# LANGUAGE FlexibleInstances, QuasiQuotes, UndecidableInstances #-}
module Gen.GenerateTypeCheckTest ( tests ) where

import Gen.Essence.St
import Gen.Essence.Spec ()
import Gen.Helpers.SizeOf
import Gen.Imports
import Gen.Reduce.Simpler
import Gen.TestPrelude
import Conjure.Language.Definition
import Conjure.Language.NameResolution (resolveNames)
import Conjure.UI.TypeCheck            (typeCheckModel)

tests :: TestTree
tests = testGroup "GenerateTypeCheck"
  [
   testGroup "QC"
   [
     qc_tests "Spec"
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

instance (Generate a) => Arbitrary (Limited a) where
  arbitrary = sized $ \s -> do
    i <- choose (1,  (max 0 (min s 2)) )
    Limited <$> runGenerateNullLogs GNone def{depth=i}


qc_tests :: String  -> TestTree
qc_tests title  =
  testGroup title $
   catMaybes $ [
     Just $ testProperty "Conjure's TypeChecking" $
       \(Limited (sp :: Spec)) ->  do
          case toConjure sp of
            Left err               -> counterexample (show err) False
            Right (model :: Model) -> do
               case typeCheck model of
                 Left err -> counterexample (show err) False
                 Right{}  -> counterexample ("Passing") True
   ]

typeCheck :: MonadFail m => Model -> m Model
typeCheck m = ignoreLogs . runNameGen  $ (resolveNames $ m) >>= typeCheckModel
