{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- A file which is only loaded when doing `make ghci`
-- To saving typing imports and storing temp defs

module Gen.ZGHCI(module X
  , d_boolrel, d_bool_func_set
  , aSpec, aExpr, aType, aDom
  ) where

import Conjure.Language.Definition    as X
import Conjure.Language.Domain        as X
import Conjure.Language.Expression.Op as X
import Conjure.Language.TH            as X
import Gen.AST.TH                     as X
import Gen.Essence.Constant           as X ()
import Gen.Essence.Domain             as X ()
import Gen.Essence.Expr               as X ()
import Gen.Essence.Id                 as X
import Gen.Essence.Literal            as X ()
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

import qualified Data.Map as M

-- Gives various types

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



--Debugging

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
