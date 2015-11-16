{-# LANGUAGE FlexibleInstances, QuasiQuotes, UndecidableInstances #-}
module Gen.GenerateTypeCheckTest ( tests ) where

import Conjure.Language.Definition
import Conjure.Language.NameResolution (resolveNames)
import Conjure.UI.TypeCheck            (typeCheckModel)
import Conjure.UserError               (MonadUserError)
import Gen.Essence.Spec                ()
import Gen.Essence.St
import Gen.Imports
import Gen.TestPrelude

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
    expr <- choose (0,  (max 1 (min s 4)) )
    dom  <- choose (1,  (max 1 (min s 3)) )
    Limited <$> runGenerateNullLogs (GDomainDepth dom) def{depth=expr}


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

typeCheck :: (MonadFail m, MonadUserError m) => Model -> m Model
typeCheck m = ignoreLogs . runNameGen  $ (resolveNames $ m) >>= typeCheckModel
