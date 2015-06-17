{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- A file which is only loaded when doing make ghci
-- Useful for saving trying imports and storing defs
module Gen.Essence.ZGHCI(module X
  , ws
  ) where

import Conjure.Language.Definition    as X
import Conjure.Language.Expression.Op as X
import Conjure.Language.TH            as X
import Gen.AST.TH                     as X
import Gen.Essence.Constant           as X ()
import Gen.Essence.Domain             as X ()
import Gen.Essence.Literal            as X ()
import Gen.Essence.Op                 as X ()
import Gen.Essence.Range              as X ()
import Gen.Essence.Rnd                as X
import Gen.Essence.St                 as X
import Gen.Essence.Type               as X ()
import Gen.Helpers.SizeOf             as X
import Gen.Helpers.TypeOf             as X
import Gen.Imports                    as X

ws = [(K_TypeAny, 0), (K_TypeBool, 100), (K_TypeEnum, 0),
                  (K_TypeFunction, 0), (K_TypeInt, 0), (K_TypeList, 0),
                  (K_TypeMSet, 0), (K_TypeMatrix, 0), (K_TypePartition, 0),
                  (K_TypeRecord, 0), (K_TypeRelation, 100), (K_TypeSequence, 0),
                  (K_TypeSet, 0), (K_TypeTuple, 0), (K_TypeUnnamed, 0),
                  (K_TypeVariant, 0)]
