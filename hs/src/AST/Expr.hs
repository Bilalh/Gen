{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Use deriving instance Show  instead of deriving Show
module AST.Expr where

import Conjure.Prelude
import Conjure.Language.Pretty
import Conjure.Language.Definition
import Conjure.Language.TH

import AST.Data
import {-# SOURCE #-} AST.Literal()
import AST.Type()
import Conjure.Language.Ops.Generated


instance Translate Expr Expression where
  fromConjure (Constant t) = ELit <$> fromConjure  t
  -- fromConjure (AbstractLiteral t)   = _f
  -- fromConjure (Domain t)            = _f
  -- fromConjure (Reference t1 t2)     = _f
  -- fromConjure (WithLocals t1 t2)    = _f
  -- fromConjure (Comprehension t1 t2) = _f
  -- fromConjure (Typed t1 t2)         = _f
  fromConjure t@(Op _) =  do
      case fromConjure t of
        Just (x :: BinOp)  -> pure $ EBinOp x
        Nothing -> do
          case fromConjure t of
            Just (x :: UniOp)  -> pure $ EUniOp x
            Nothing -> fromConjureFail "Expr Expression" t


  -- fromConjure (ExpressionMetaVar t) =
  fromConjure x = fromConjureFail "Expr Expression" x

  toConjure (ELit x)            = Constant <$> toConjure x
  -- toConjure (EVar x)            =  _t
  -- toConjure (EQVar x)           =  _t
  toConjure (EBinOp x)          =  toConjure x
  -- toConjure (EUniOp x)          =  _t
  -- toConjure (EProc x)           =  _t
  -- toConjure (EDom x)            =  _t
  -- toConjure (ETyped x1 x2)      =  _t
  -- toConjure EEmptyGuard         =  _t
  -- toConjure (EQuan x1 x2 x3 x4) =  _t

  toConjure x = toConjureFail "Expr Expression" x

instance Translate UniOp Expression where
   fromConjure [essence| |&x|  |] = UBar <$> fromConjure x
   fromConjure [essence| -&x |]   = UNeg <$> fromConjure x

   fromConjure x = fromConjureFail "UniOp Expression" x

   toConjure (UBar x) = pure [essence| |&x'| |] where
       x' = toConjureNote "UniOp expr" x
   toConjure (UNeg x) = pure  [essence| -&x' |] where
       x' = toConjureNote "UniOp Expr" x



instance Translate BinOp Expression where
    fromConjure [essence| &x in &y |]        = BIn        <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x - &y |]         = BDiff      <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x = &y |]         = BEQ        <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x != &y |]        = BNEQ       <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x < &y |]         = BLT        <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x <= &y |]        = BLTE       <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x > &y |]         = BGT        <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x >= &y |]        = BGTE       <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x + &y |]         = BPlus      <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x * &y |]         = BMult      <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x / &y |]         = BDiv       <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x ** &y |]        = BPow       <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x % &y |]         = BMod       <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x /\ &y |]        = BAnd       <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x \/ &y |]        = BOr        <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x -> &y |]        = Bimply     <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x <-> &y |]       = Biff       <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x subset &y |]    = Bsubset    <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x subsetEq &y |]  = BsubsetEq  <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x supset &y |]    = Bsupset    <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x supsetEq &y |]  = BsupsetEq  <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x intersect &y |] = Bintersect <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x union &y |]     = Bunion     <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x <lex &y |]      = BlexLT     <$> fromConjure x <*> fromConjure y
    fromConjure [essence| &x <=lex &y |]     = BlexLTE    <$> fromConjure x <*> fromConjure y
    -- fromConjure [essence| &x >lex &y |]      = BlexGT     <$> fromConjure x <*> fromConjure y
    -- fromConjure [essence| &x >=lex &y |]     = BlexGTE    <$> fromConjure x <*> fromConjure y


    fromConjure x = fromConjureFail "BinOp Expression" x


    toConjure (BIn x y) = return [essence| &x' in &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BDiff x y) = return [essence| &x' - &y' |] where
            x' = toConjureNote "toConjure binOP" x
            y' = toConjureNote "toConjure binOP" y

    toConjure (BEQ x y) = return [essence| &x' = &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BNEQ x y) = return [essence| &x' != &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y


    toConjure (BLT x y) = return [essence| &x' < &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BLTE x y) = return [essence| &x' <= &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BGT x y) = return [essence| &x' > &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BGTE x y) = return [essence| &x' >= &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y


    toConjure (BPlus x y) = return [essence| &x' + &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BMult x y) = return [essence| &x' * &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BDiv x y) = return [essence| &x' / &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BPow x y) = return [essence| &x' ** &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BMod x y) = return [essence| &x' % &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BAnd x y) = return [essence| &x' /\ &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BOr x y) = return [essence| &x' \/ &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y


    toConjure (Bimply x y) = return [essence| &x' -> &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (Biff x y) = return [essence| &x' <-> &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y


    toConjure (Bsubset x y) = return [essence| &x' subset &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BsubsetEq x y) = return [essence| &x' subsetEq &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (Bsupset x y) = return [essence| &x' supset &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BsupsetEq x y) = return [essence| &x' supsetEq &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (Bintersect x y) = return [essence| &x' intersect &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (Bunion x y) = return [essence| &x' union &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BlexLT x y) = return [essence| &x' <lex &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BlexLTE x y) = return [essence| &x' <=lex &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BlexGT x y) = return [essence| &x' >lex &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure (BlexGTE x y) = return [essence| &x' >=lex &y' |] where
        x' = toConjureNote "toConjure binOP" x
        y' = toConjureNote "toConjure binOP" y

    toConjure x@(BOver _ _ ) = fail $ "toConjure Bover missing case" <+> (pretty $ groom x)

    -- toConjure x = toConjureFail "Expr Expression" x



instance Pretty Expr where
  pretty = pretty . (toConjureNote "Pretty Expr" :: Expr -> Expression)

instance Pretty BinOp where
  pretty = pretty . (toConjureNote "Pretty BinOp" :: BinOp -> Expression)

instance Pretty UniOp where
  pretty = pretty . (toConjureNote "Pretty UniOP" :: UniOp -> Expression)

instance Pretty Proc where
    -- pretty =   pretty  . toEssence

instance Pretty QType where
    -- pretty =   pretty  . toEssence
