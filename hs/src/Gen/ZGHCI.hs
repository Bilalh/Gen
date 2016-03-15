{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- A file which is only loaded when doing `make ghci`
-- To saving typing imports and storing temp defs
-- Usage:  make ghci    import Gen.ZGHCI
module Gen.ZGHCI(module X
  , d_boolrel, d_bool_func_set, d_func_size, d_func_size2, d_func_size3
  , aSpec, aExpr, aType, aDom, aBinRel, aIntRanged
  , the_comp, the_comp_gens, l_mat, the_opp, the_oppe, the_and
  ) where

import Conjure.Language.Definition    as X
import Conjure.Language.Domain        as X
import Conjure.Language.Expression.Op as X
import Conjure.Language.TH            as X
import Conjure.UI.IO                  as X
import Gen.AST.TH                     as X
import Gen.Essence.Constant           as X ()
import Gen.Essence.Domain             as X ()
import Gen.Essence.Expr               as X ()
import Gen.Essence.Id                 as X
import Gen.Essence.Ints               as X
import Gen.Essence.Literal            as X ()
import Gen.Essence.Log                as X
import Gen.Essence.Op                 as X ()
import Gen.Essence.Range              as X ()
import Gen.Essence.Rnd                as X
import Gen.Essence.Spec               as X ()
import Gen.Essence.St                 as X
import Gen.Essence.Type               as X ()
import Gen.Helpers.SizeOf             as X
import Gen.Helpers.TypeOf             as X
import Gen.Imports                    as X
import Gen.IO.Formats                 as X
import Gen.IO.RunResult               as X
import Gen.IO.ToolchainData           as X
import Gen.Reduce.Reduction           as X
import Gen.Reduce.Simpler             as X
import System.Environment             as X (lookupEnv, setEnv)

import qualified Data.Map as M

-- Gives various expressions
-- Usage <func>  def{depth=<n>}

aSpec :: St -> IO Spec
aSpec st = do
  runGenerate2 LogNone (give GNone) st

aExpr :: St -> IO Expr
aExpr st = do
  ty <- runGenerate2 LogNone (give GNone) st
  runGenerate2 LogNone (give (GType ty )) st

aDom :: St -> IO (Domain () Expr)
aDom st = do
  runGenerate2 LogNone (give GNone) st

aType :: St -> IO Type
aType st = do
  runGenerate2 LogNone (give GNone) st


aBinRel :: St -> IO (Domain () Expr)
aBinRel st = do
  ty <- runGenerate2 LogNone (dgive GNone) st
  let rel_ty = TypeRelation [ty,ty]
  runGenerate2 LogNone (give $ GType rel_ty) st

aIntRanged :: St -> Integer -> Integer -> IO (IntRanged Expr)
aIntRanged st a b = do
  runGenerate2 LogNone (give $ GIntRanged a b) st

--Debugging


l_mat = (pretty :: AbstractLiteral Constant -> Doc  ) <$>
   runGenerate2 LogDebugVerbose ( give con ) def{depth=4, weighting=def}
  where con = GType (TypeMatrix TypeInt (TypeMatrix TypeInt (TypeMatrix TypeInt TypeInt)))

w_boolrel = [(K_TypeAny, 0), (K_TypeBool, 100), (K_TypeEnum, 0),
                  (K_TypeFunction, 0), (K_TypeInt, 0), (K_TypeList, 0),
                  (K_TypeMSet, 0), (K_TypeMatrix, 0), (K_TypePartition, 0),
                  (K_TypeRecord, 0), (K_TypeRelation, 100), (K_TypeSequence, 0),
                  (K_TypeSet, 0), (K_TypeTuple, 0), (K_TypeUnnamed, 0),
                  (K_TypeVariant, 0)]

d_boolrel = (pretty :: Domain () Expr -> Doc  ) <$>
   runGenerate2 LogNone ( withKey K_Domain $ give con  ) def{depth=2, weighting=KeyMap $ M.fromList w_boolrel}
  where con = GType (TypeRelation [TypeRelation [TypeBool, TypeBool, TypeBool], TypeBool])

w_bool_func_set = [(K_TypeAny, 0), (K_TypeBool, 100), (K_TypeEnum, 0),
                  (K_TypeFunction, 100), (K_TypeInt, 0), (K_TypeList, 0),
                  (K_TypeMSet, 0), (K_TypeMatrix, 0), (K_TypePartition, 0),
                  (K_TypeRecord, 0), (K_TypeRelation, 0), (K_TypeSequence, 0),
                  (K_TypeSet, 100), (K_TypeTuple, 0), (K_TypeUnnamed, 0),
                  (K_TypeVariant, 0)]

d_bool_func_set = (pretty :: Domain () Expr -> Doc  ) <$>
   runGenerate2 LogNone (  withKey K_Domain $ give con  ) def{depth=2, weighting=KeyMap $ M.fromList w_bool_func_set}
  where con = GType (TypeSet (TypeSet TypeBool))


w_func_size= [(K_TypeAny, 0), (K_TypeBool, 100), (K_TypeEnum, 0),
             (K_TypeFunction, 100), (K_TypeInt, 100), (K_TypeList, 0),
             (K_TypeMSet, 0), (K_TypeMatrix, 0), (K_TypePartition, 0),
             (K_TypeRecord, 0), (K_TypeRelation, 0), (K_TypeSequence, 0),
             (K_TypeSet, 0), (K_TypeTuple, 0), (K_TypeUnnamed, 0)
             ] ++ [ (k,0) |
               k <- [ K_JectivityAttr_Bijective,K_JectivityAttr_Injective
                    , K_JectivityAttr_Surjective, K_PartialityAttr_Total
                    , K_OpFactorial
                    ]]

-- Try to generate a function given a depth of 3 which actually has a depth of 4.
d_func_size = (pretty :: (Integer, Domain () Expr) -> Doc ) . (depthOf &&& id) <$>
   runGenerate2 LogNone (  withKey K_Domain $ give con  ) def{depth=3, weighting=KeyMap $ M.fromList w_func_size}
  where con = GType (TypeFunction TypeBool TypeBool)

d_func_size2 = do
   (res:: Domain () Expr ,lgs) <- runGenerate LogDebug ( withKey K_Domain $ give con )
     def{depth=givenDepth, weighting=KeyMap $ M.fromList w_func_size}
   if depthOf res > fromIntegral givenDepth then do
     return $ Just (res, lgs, res)
   else
     return Nothing

  where con = GType (TypeFunction TypeBool TypeBool)
        givenDepth = 3

d_func_size3 = mapM (const d_func_size2) [1..1000] >>= return . take 1 . catMaybes >>= return . map pretty
--


the_comp = EComp (ECon (ConstantBool True))
              [GenDom (Single (Name "m1"))
                 (DomainInt
                    [RangeBounded (ECon (ConstantInt 1)) (ECon (ConstantInt 3))]),
               GenDom (Single (Name "q5"))
                 (DomainInt
                    [RangeBounded (ECon (ConstantInt 1)) (ECon (ConstantInt 2))]),
               GenDom (Single (Name "t1"))
                 (DomainInt
                    [RangeBounded (ECon (ConstantInt 0))
                       (EOp
                          (MkOpIndexing
                             (OpIndexing
                                (ELit
                                   (AbsLitMatrix
                                      (DomainInt
                                         [RangeBounded (ECon (ConstantInt 1))
                                            (ECon (ConstantInt 1))])
                                      [ECon (ConstantInt 4)]))
                                (EVar (Var "q5" TypeInt)))))])]
              []

the_comp_gens = [GenDom (Single (Name "m1"))
                 (DomainInt
                    [RangeBounded (ECon (ConstantInt 1)) (ECon (ConstantInt 3))]),
               GenDom (Single (Name "q5"))
                 (DomainInt
                    [RangeBounded (ECon (ConstantInt 1)) (ECon (ConstantInt 2))]),
               GenDom (Single (Name "t1"))
                 (DomainInt
                    [RangeBounded (ECon (ConstantInt 0))
                       (EOp
                          (MkOpIndexing
                             (OpIndexing
                                (ELit
                                   (AbsLitMatrix
                                      (DomainInt
                                         [RangeBounded (ECon (ConstantInt 1))
                                            (ECon (ConstantInt 1))])
                                      [ECon (ConstantInt 4)]))
                                (EVar (Var "q5" TypeInt)))))])]


the_oppe = EOp the_opp
the_opp  = (MkOpEq
             (OpEq
                (ELit
                   (AbsLitMSet
                      [ECon (ConstantBool False), ECon (ConstantBool False)]))
                (ECon
                   (ConstantAbstract
                      (AbsLitMSet [ConstantBool True, ConstantBool False])))))

the_and = EOp
     (MkOpAnd
        (OpAnd
           (EComp
              (EOp
                 (MkOpAllDiff
                    (OpAllDiff
                       (EOp
                          (MkOpIndexing
                             (OpIndexing
                                (EOp
                                   (MkOpIndexing
                                      (OpIndexing
                                         (EVar
                                            (Var "var1"
                                               (TypeMatrix TypeInt
                                                  (TypeMatrix TypeInt
                                                     (TypeMatrix TypeInt TypeInt)))))
                                         (ECon (ConstantInt 2)))))
                                (EVar (Var "i" TypeInt))))))))
              [GenDom (Single (Name "i"))
                 (DomainInt [RangeSingle (ECon (ConstantInt 5))])]
              [])))