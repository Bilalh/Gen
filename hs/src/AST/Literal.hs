{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module AST.Literal where

import Conjure.Prelude
import Conjure.Language.Pretty
import Conjure.Language.Definition


import AST.Data
import {-# SOURCE #-} AST.Expr()
import AST.Domain()


instance Translate Literal Expression where
    toConjure y@(EB _) = pure Constant <*> toConjure y
    toConjure y@(EI _) = pure Constant <*> toConjure y
    toConjure (EExpr x) = toConjure x
    toConjure x        = pure AbstractLiteral <*> toConjure x
    -- toConjure x = toConjureFail "Literal Expression" x


    fromConjure (AbstractLiteral x) = fromConjure x
    fromConjure (Constant x) = fromConjure x
    fromConjure x = pure EExpr <*> fromConjure x

instance Translate Literal (AbstractLiteral Expression)  where
    fromConjure (AbsLitTuple x)       = pure ETuple     <*> mapM fromConjure x
    fromConjure (AbsLitMatrix dom vs) = pure EMatrix    <*> mapM fromConjure vs
                                                        <*> fromConjure dom
    fromConjure (AbsLitSet x)         = pure ESet       <*> mapM fromConjure x
    fromConjure (AbsLitMSet x)        = pure EMSet      <*> mapM fromConjure x
    fromConjure (AbsLitFunction x)    = pure EFunction  <*> mapM fromConjure x
    -- fromConjure (AbsLitRelation x) = _d
    fromConjure (AbsLitPartition x)   = pure EPartition <*> mapM (mapM fromConjure) x

    fromConjure x = fromConjureFail "Literal (AbstractLiteral Expression)" x

    -- toConjure (EB x)           =  _f
    -- toConjure (EI x)           = _f
    toConjure (ETuple x)       = pure AbsLitTuple     <*> mapM toConjure x
    toConjure (EMatrix vs dom) = pure AbsLitMatrix    <*> toConjure dom
                                                      <*> mapM toConjure vs
    toConjure (ESet x)         = pure AbsLitSet       <*> mapM toConjure x
    toConjure (EMSet x)        = pure AbsLitMSet      <*> mapM toConjure x
    toConjure (EFunction x)    = pure AbsLitFunction  <*> mapM toConjure x
    -- toConjure (ERelation x) = pure AbsLitRelation  <*> mapM toConjure x
    toConjure (EPartition x)   = pure AbsLitPartition <*> mapM (mapM toConjure) x
    -- toConjure (EExpr x)     = pure AbstractLiteral <*> toConjure x

    toConjure x = toConjureFail "Literal (AbstractLiteral Expression)" x


instance Translate Literal Constant where
  fromConjure (ConstantBool r)        = return $ EB r
  fromConjure (ConstantInt r)         = return $ EI (fromIntegral r)
  fromConjure x = fromConjureFail "Literal Constant" x

  -- fromConjure (ConstantEnum r1 r2 r3) = _r
  -- fromConjure (ConstantAbstract r)    = _r
  -- fromConjure (DomainInConstant r)    = _r
  -- fromConjure (ConstantUndefined r)   = _r

  toConjure (EB x) = pure $ ConstantBool x
  toConjure (EI x) = pure $ ConstantInt (fromInteger x)
  toConjure x = toConjureFail "Literal Constant" x

instance Pretty Literal where
    pretty (EB x) = pretty $ ConstantBool x
    pretty (EI x) = pretty $ ConstantInt (fromInteger x)
    pretty x = error . renderNormal $ ("Pretty Literal " <+>  (pretty . groom) x)

instance (Translate a b) => Translate (a,a) (b,b) where
    toConjure (x,y) = do
      xx <- toConjure x
      yy <- toConjure y
      return (xx, yy)

    fromConjure (x,y) = do
      xx <- fromConjure x
      yy <- fromConjure y
      return (xx, yy)
