{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Domain where

import Gen.Essence.St
import Gen.Helpers.StandardImports
import Gen.Essence.Rnd
import Gen.Essence.Type()
import Gen.Essence.Range()
import Conjure.Language.Definition
import Conjure.Language.Domain
import Gen.Helpers.SizeOf
import Gen.Essence.Literal()

instance (Generate a, WrapConstant a) => Generate (Domain () a) where
  give GNone = do
      -- ty <- give GNone
      give (GType TInt)

  give (GType TBool)           = return DomainBool
  give (GType TInt)            = pure DomainInt <*> vectorOf3 2 (give GNone)
  -- give (GType (TMatix ty))     = _x
  -- give (GType (TSet ty))       = _x
  -- give (GType (TMSet ty))      = _x
  -- give (GType (TFunc ty1 ty2)) = _x
  -- give (GType (TTuple ty))     = _x
  -- give (GType (TRel ty))       = _x
  -- give (GType (TPar ty))       = _x
  -- give (GType (TUnamed ty))    = _x
  -- give (GType (TEnum ty))      = _x
  -- give (GType TAny)            = _x

  give t = giveUnmatched "Generate (Domain () a)" t

  possiblePure _ _ _ = True
