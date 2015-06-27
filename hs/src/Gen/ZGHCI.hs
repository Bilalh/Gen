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
  , chs
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
import Gen.Reduce.Reduction           as X
import Gen.Reduce.Simpler             as X


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

chs = sort
    [ "/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_00-53_1435103612/_errors/RefineParam_/ErrorUnknown_/1435104353_54_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_00-53_1435103612/_errors/RefineParam_/ErrorUnknown_/1435104353_54_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_00-53_1435103612/_errors/RefineParam_/ErrorUnknown_/1435104353_54_r-.model000002.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_00-53_1435103612/_errors/RefineParam_/ErrorUnknown_/1435104353_54_r-.model000005.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineCompact_/ErrorUnknown_/1435108111_68_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineCompact_/ErrorUnknown_/1435110669_49_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineCompact_/ErrorUnknown_/1435111909_80_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineCompact_/ErrorUnknown_/1435113199_56_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineCompact_/ErrorUnknown_/1435115631_19_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineCompact_/NotRefined_/1435105436_17_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineCompact_/NotRefined_/1435111218_35_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineCompact_/NotRefined_/1435130298_94_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineCompact_/RuleApplication_/1435108706_20_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineParam_/ErrorUnknown_/1435118026_10_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineParam_/ErrorUnknown_/1435118026_10_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineParam_/ErrorUnknown_/1435118026_10_r-.model000002.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineParam_/ErrorUnknown_/1435118026_10_r-.model000007.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineParam_/ErrorUnknown_/1435118026_10_r-.model000009.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineParam_/ErrorUnknown_/1435120775_54_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineParam_/ErrorUnknown_/1435120775_54_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineParam_/ErrorUnknown_/1435120775_54_r-.model000002.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineParam_/ErrorUnknown_/1435124092_48_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineParam_/ErrorUnknown_/1435124092_48_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineParam_/ParseError_/1435115110_54_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineParam_/ParseError_/1435115110_54_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineParam_/ParseError_/1435115423_85_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineParam_/ParseError_/1435115423_85_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/RefineParam_/ParseError_/1435115423_85_r-.model000003.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/Savilerow_/TypeChecking_/1435114959_70_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435110917_43_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435110917_43_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435112072_16_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435112072_16_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435115789_99_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435115789_99_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435120078_64_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435120078_64_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435122746_74_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435122746_74_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435124021_47_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435124021_47_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435124021_47_r-.model000003.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435126163_13_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435126163_13_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435126163_13_r-.model000002.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435126163_13_r-.model000003.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435126163_13_r-.model000020.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435128324_57_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-3/2015-06-24_01-21_1435105274/_errors/TranslateUp_/ErrorUnknown_/1435128324_57_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineCompact_/ErrorUnknown_/1435131671_70_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineCompact_/ErrorUnknown_/1435136957_30_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineCompact_/ErrorUnknown_/1435137783_31_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineCompact_/ErrorUnknown_/1435138977_10_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineCompact_/ErrorUnknown_/1435141453_99_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineCompact_/ErrorUnknown_/1435142908_25_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineCompact_/ErrorUnknown_/1435143400_30_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineCompact_/ErrorUnknown_/1435148451_38_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineCompact_/NotRefined_/1435132020_93_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineCompact_/NotRefined_/1435138340_66_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineCompact_/NotRefined_/1435140010_44_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineCompact_/NotRefined_/1435141939_71_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineCompact_/NotRefined_/1435147026_26_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineCompact_/NotRefined_/1435159471_47_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineParam_/ErrorUnknown_/1435145550_10_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineParam_/ErrorUnknown_/1435145550_10_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineParam_/ErrorUnknown_/1435151034_51_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineParam_/ErrorUnknown_/1435151034_51_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineParam_/ParseError_/1435136619_87_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineParam_/ParseError_/1435136619_87_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineParam_/ParseError_/1435136619_87_r-.model000002.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineParam_/ParseError_/1435159192_27_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/RefineParam_/ParseError_/1435159192_27_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/Savilerow_/JavaException_/1435135718_54_r-.model000002.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/Savilerow_/JavaException_/1435135718_54_r-.model000010.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/Savilerow_/JavaException_/1435151463_98_r-.model000003.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/Savilerow_/JavaException_/1435151463_98_r-.model000005.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/Savilerow_/JavaException_/1435151463_98_r-.model000009.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/Savilerow_/JavaException_/1435151463_98_r-.model000011.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/Savilerow_/JavaException_/1435151463_98_r-.model000012.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/Savilerow_/JavaException_/1435151463_98_r-.model000014.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/Savilerow_/TypeChecking_/1435141546_33_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/Savilerow_/TypeChecking_/1435148063_62_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/Savilerow_/TypeChecking_/1435148063_62_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435131843_30_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435131843_30_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435132619_71_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435132619_71_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435132619_71_r-.model000003.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435132619_71_r-.model000006.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435132619_71_r-.model000010.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435135567_26_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435135567_26_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435135567_26_r-.model000002.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435139033_79_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435139033_79_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435146705_18_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435146705_18_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435146705_18_r-.model000002.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435147474_83_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435147474_83_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435148859_32_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435148859_32_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435149001_48_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-4/2015-06-24_08-41_1435131669/_errors/TranslateUp_/ErrorUnknown_/1435149001_48_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineCompact_/ErrorUnknown_/1435167647_64_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineCompact_/ErrorUnknown_/1435173634_78_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineCompact_/ErrorUnknown_/1435179182_13_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineCompact_/ErrorUnknown_/1435179222_51_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineCompact_/ErrorUnknown_/1435181643_53_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineCompact_/NotRefined_/1435178167_41_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineCompact_/NotRefined_/1435189649_96_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineCompact_/RuleApplication_/1435170206_44_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineCompact_/RuleApplication_/1435172423_41_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineParam_/ErrorUnknown_/1435170503_96_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineParam_/ErrorUnknown_/1435170503_96_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineParam_/ParseError_/1435183319_47_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineParam_/ParseError_/1435183319_47_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineParam_/ParseError_/1435183319_47_r-.model000002.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineParam_/ParseError_/1435183319_47_r-.model000003.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineParam_/ParseError_/1435183319_47_r-.model000004.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineParam_/ParseError_/1435183319_47_r-.model000006.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineParam_/ParseError_/1435183319_47_r-.model000009.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineParam_/ParseError_/1435183319_47_r-.model000027.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineParam_/ParseError_/1435183319_47_r-.model000031.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineRandom_/NotRefined_/1435178167_41_r-.model000002.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/RefineRandom_/NotRefined_/1435178167_41_r-.model000003.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/Savilerow_/JavaException_/1435176815_24_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/Savilerow_/JavaException_/1435176815_24_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/Savilerow_/JavaException_/1435179480_84_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/Savilerow_/JavaException_/1435179480_84_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/Savilerow_/JavaException_/1435186404_14_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/Savilerow_/JavaException_/1435186404_14_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/Savilerow_/JavaException_/1435188262_37_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/Savilerow_/JavaException_/1435188262_37_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/Savilerow_/ParseError_/1435170594_38_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/Savilerow_/TypeChecking_/1435167496_86_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/Savilerow_/TypeChecking_/1435168752_70_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/Savilerow_/TypeChecking_/1435169441_31_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/Savilerow_/TypeChecking_/1435169954_27_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/Savilerow_/TypeChecking_/1435194081_33_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/TranslateUp_/ErrorUnknown_/1435168260_98_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/TranslateUp_/ErrorUnknown_/1435168260_98_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/TranslateUp_/ErrorUnknown_/1435169684_14_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/TranslateUp_/ErrorUnknown_/1435169684_14_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/TranslateUp_/ErrorUnknown_/1435170550_64_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/TranslateUp_/ErrorUnknown_/1435170550_64_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/TranslateUp_/ErrorUnknown_/1435173201_60_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/TranslateUp_/ErrorUnknown_/1435173201_60_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/TranslateUp_/ErrorUnknown_/1435173350_51_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/TranslateUp_/ErrorUnknown_/1435173350_51_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/TranslateUp_/ErrorUnknown_/1435173350_51_r-.model000002.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/TranslateUp_/ErrorUnknown_/1435176366_42_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/TranslateUp_/ErrorUnknown_/1435176366_42_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/TranslateUp_/ErrorUnknown_/1435176366_42_r-.model000002.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-2/expr-5/2015-06-24_18-29_1435166957/_errors/TranslateUp_/ErrorUnknown_/1435176366_42_r-.model000005.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineCompact_/ErrorUnknown_/1435199148_87_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineCompact_/ErrorUnknown_/1435201978_93_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineCompact_/ErrorUnknown_/1435204425_37_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineCompact_/ErrorUnknown_/1435209539_80_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineCompact_/ErrorUnknown_/1435209810_77_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineCompact_/ErrorUnknown_/1435211647_75_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineCompact_/ErrorUnknown_/1435216799_17_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineCompact_/ErrorUnknown_/1435217333_51_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineCompact_/ErrorUnknown_/1435217595_73_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineCompact_/NotRefined_/1435201550_65_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineCompact_/NotRefined_/1435208191_85_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineCompact_/NotRefined_/1435212692_35_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineCompact_/NotRefined_/1435212982_68_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineCompact_/NotRefined_/1435213364_36_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineCompact_/NotRefined_/1435218991_41_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineParam_/ParseError_/1435202605_25_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineParam_/ParseError_/1435202605_25_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineParam_/ParseError_/1435215133_84_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineParam_/ParseError_/1435215133_84_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineRandom_/ErrorUnknown_/1435201978_93_r-.model000003.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineRandom_/ErrorUnknown_/1435211647_75_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineRandom_/ErrorUnknown_/1435211647_75_r-.model000008.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/RefineRandom_/NotRefined_/1435208191_85_r-.model000006.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/Savilerow_/JavaException_/1435197416_88_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/Savilerow_/JavaException_/1435197416_88_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/Savilerow_/JavaException_/1435199646_88_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/Savilerow_/JavaException_/1435199646_88_r-.model000002.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/Savilerow_/TypeChecking_/1435196486_42_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/TranslateUp_/ErrorUnknown_/1435196034_71_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/TranslateUp_/ErrorUnknown_/1435196034_71_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/TranslateUp_/ErrorUnknown_/1435196034_71_r-.model000004.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/TranslateUp_/ErrorUnknown_/1435210746_97_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/TranslateUp_/ErrorUnknown_/1435210746_97_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/Validate_/ErrorUnknown_/1435204916_29_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/Validate_/ErrorUnknown_/1435204916_29_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/Validate_/ErrorUnknown_/1435206883_12_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/Validate_/ErrorUnknown_/1435206883_12_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/Validate_/ErrorUnknown_/1435209952_52_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-3/2015-06-25_02-25_1435195557/_errors/Validate_/ErrorUnknown_/1435209952_52_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-4/2015-06-25_09-06_1435219582/_errors/RefineCompact_/ErrorUnknown_/1435219701_19_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-4/2015-06-25_09-06_1435219582/_errors/RefineCompact_/NotRefined_/1435219619_10_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-4/2015-06-25_09-06_1435219582/_errors/Savilerow_/JavaException_/1435221027_27_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-4/2015-06-25_09-06_1435219582/_errors/Savilerow_/JavaException_/1435221027_27_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-4/2015-06-25_09-06_1435219582/_errors/Savilerow_/JavaException_/1435221027_27_r-.model000006.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-4/2015-06-25_09-06_1435219582/_errors/TranslateUp_/ErrorUnknown_/1435222515_47_r-.model000000.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-4/2015-06-25_09-06_1435219582/_errors/TranslateUp_/ErrorUnknown_/1435222515_47_r-.model000001.choices/final"
  ,"/Users/bilalh/Desktop/Results/_notable/_new/sizes/dom-3/expr-5/2015-06-25_10-01_1435222862/_errors/Savilerow_/JavaException_/1435223288_15_r-.model000001.choices/final"
  ]
