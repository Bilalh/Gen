{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AST.Expr where

import Conjure.Prelude
import Conjure.Language.Pretty
import Conjure.Language.Definition
import Conjure.Language.TH

import AST.Data
import {-# SOURCE #-} AST.Literal()
import AST.Type()
import AST.Domain()

instance Translate Expr Expression where
  fromConjure (Constant t)             = ELit   <$> fromConjure t
  fromConjure (AbstractLiteral t)      = ELit   <$> fromConjure t
  fromConjure (Domain t)               = EDom   <$> fromConjure t
  fromConjure (Reference t1 _)         = EVar   <$> fromConjure t1
  -- fromConjure (WithLocals t1 t2)    = _f
  -- fromConjure (Comprehension t1 t2) = _f
  fromConjure (Typed t1 t2)            = ETyped <$> fromConjure t2 <*> fromConjure t1

  fromConjure t@(Op _) =  do
      case fromConjure t of
        Just (x :: BinOp)  -> pure $ EBinOp x
        Nothing -> do
          case fromConjure t of
            Just (x :: UniOp)  -> pure $ EUniOp x
            Nothing -> do
              case fromConjure t of
                Just (x :: Proc)  -> pure $ EProc x
                Nothing -> fromConjureFail "Expr Expression" t


  -- fromConjure (ExpressionMetaVar t) =
  fromConjure x = fromConjureFail "Expr Expression" x


  toConjure (ELit t) =  do
      case toConjure t of
        Just (x :: Constant)  -> pure $ Constant x
        Nothing -> do
          case toConjure t of
            Just (x :: AbstractLiteral Expression)  -> pure $ AbstractLiteral x
            Nothing -> toConjureFail "Expr Expression" t

  -- toConjure (ELit x@(EB _))        =  Constant        <$>  toConjure x
  -- toConjure (ELit x)               =  AbstractLiteral <$>  toConjure x

  toConjure (EVar x)               =  Reference <$> toConjure x <*> return Nothing --FIXME correct?
  toConjure (EBinOp x)             =  toConjure x
  toConjure (EUniOp x)             =  toConjure x
  toConjure (EProc x)              =  toConjure x
  toConjure (EDom x)               =  Domain <$> toConjure x
  toConjure (ETyped x1 x2)         =  Typed <$> toConjure x2 <*> toConjure x1
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


instance Translate Proc Expression where
  fromConjure [essence| &ele[&indexer] |] = Pindex <$> fromConjure ele <*> fromConjure indexer

  fromConjure [essence| allDiff(&x) |]          = PallDiff     <$> fromConjure x
  fromConjure [essence| freq(&x, &y) |]         = Pfreq        <$> fromConjure x <*> fromConjure y
  -- fromConjure [essence| hist(&x, &y) |]         = Phist        <$> fromConjure x <*> fromConjure y
  fromConjure [essence| min(&x) |]              = Pmin         <$> fromConjure x
  fromConjure [essence| max(&x) |]              = Pmax         <$> fromConjure x
  fromConjure [essence| toInt(&x) |]            = PtoInt       <$> fromConjure x
  fromConjure [essence| toMSet(&x) |]           = PtoMSet      <$> fromConjure x
  fromConjure [essence| toRelation(&x) |]       = PtoRelation  <$> fromConjure x
  fromConjure [essence| toSet(&x) |]            = PtoSet       <$> fromConjure x
  fromConjure [essence| defined(&x) |]          = Pdefined     <$> fromConjure x
  fromConjure [essence| image(&x, &y) |]        = Pimage       <$> fromConjure x <*> fromConjure y
  -- fromConjure [essence| inverse(&x, &y) |]      = Pinverse     <$> fromConjure x <*> fromConjure y
  fromConjure [essence| preImage(&x, &y) |]     = PpreImage    <$> fromConjure x <*> fromConjure y
  fromConjure [essence| range(&x) |]            = Prange       <$> fromConjure x
  fromConjure [essence| apart(&x, &y, &z) |]    = Papart       <$> fromConjure x <*> fromConjure y
                                                               <*> fromConjure z
  fromConjure [essence| parts(&x) |]            = Pparts       <$> fromConjure x
  fromConjure [essence| party(&x, &y) |]        = Pparty       <$> fromConjure x <*> fromConjure y
  fromConjure [essence| participants(&x) |]     = Pparticipants<$> fromConjure x
  fromConjure [essence| together(&x, &y, &z) |] = Ptogether    <$> fromConjure x <*> fromConjure y
                                                               <*> fromConjure z
  fromConjure x = fromConjureFail "Proc Expression" x


  toConjure (PallDiff x ) = return [essence| allDiff(&x') |] where
     x' = toConjureNote "Proc Expression" x

  toConjure (Pindex ref@(EVar _) c ) = return [essence| &ref'[&c']  |] where
     ref' = toConjureNote "Proc Expression" ref
     c'   = toConjureNote "Proc Expression" c

  toConjure (Pindex inn c ) = return [essence| &inner'[&c']  |] where
     inner' = toConjureNote "Proc Expression" inn
     c'     = toConjureNote "Proc Expression" c


  -- toConjure (Papply ref@(EVar _) es ) =
  --    [xMake| functionApply.actual := [ref']
  --          | functionApply.args   := es' |]
  --    where
  --    ref' = toConjureNote "Proc Expression" ref
  --    es'  = map toConjure es


  toConjure (Pfreq x y) = return [essence| freq(&x', &y') |] where
     x' = toConjureNote "Proc Expression" x
     y' = toConjureNote "Proc Expression" y

  -- toConjure (Phist x y) = return [essence| hist(&x', &y') |] where
  --    x' = toConjureNote "Proc Expression" x
  --    y' = toConjureNote "Proc Expression" y


  toConjure (Pmin x) = return [essence| min(&x') |] where
     x' = toConjureNote "Proc Expression" x

  toConjure (Pmax x) = return [essence| max(&x') |] where
     x' = toConjureNote "Proc Expression" x


  toConjure (PtoInt x) = return [essence| toInt(&x') |] where
     x' = toConjureNote "Proc Expression" x

  toConjure (PtoMSet x) = return [essence| toMSet(&x') |] where
     x' = toConjureNote "Proc Expression" x

  toConjure (PtoRelation x) = return [essence| toRelation(&x') |] where
     x' = toConjureNote "Proc Expression" x

  toConjure (PtoSet x) = return [essence| toSet(&x') |] where
     x' = toConjureNote "Proc Expression" x


  toConjure (Pdefined x) = return [essence| defined(&x') |] where
     x' = toConjureNote "Proc Expression" x

  toConjure (Pimage x y) = return [essence| image(&x', &y') |] where
     x' = toConjureNote "Proc Expression" x
     y' = toConjureNote "Proc Expression" y

  -- toConjure (Pinverse x y) = return [essence| inverse(&x', &y') |] where
  --    x' = toConjureNote "Proc Expression" x
  --    y' = toConjureNote "Proc Expression" y

  toConjure (PpreImage x y) = return [essence| preImage(&x', &y') |] where
     x' = toConjureNote "Proc Expression" x
     y' = toConjureNote "Proc Expression" y

  toConjure (Prange x) = return [essence| range(&x') |] where
     x' = toConjureNote "Proc Expression" x


  toConjure (Papart x y z) = return [essence| apart(&x', &y', &z') |] where
     x' = toConjureNote "Proc Expression" x
     y' = toConjureNote "Proc Expression" y
     z' = toConjureNote "Proc Expression" z

  toConjure (Pparts x) = return [essence| parts(&x') |] where
     x' = toConjureNote "Proc Expression" x

  toConjure (Pparty x y) = return [essence| party(&x', &y') |] where
     x' = toConjureNote "Proc Expression" x
     y' = toConjureNote "Proc Expression" y

  toConjure (Pparticipants x) = return [essence| participants(&x') |] where
     x' = toConjureNote "Proc Expression" x

  toConjure (Ptogether x y z) = return [essence| together(&x', &y', &z') |] where
     x' = toConjureNote "Proc Expression" x
     y' = toConjureNote "Proc Expression" y
     z' = toConjureNote "Proc Expression" z

  toConjure x = toConjureFail "Proc Expression" x

instance Pretty Expr where
  pretty = pretty . (toConjureNote "Pretty Expr" :: Expr -> Expression)

instance Pretty BinOp where
  pretty = pretty . (toConjureNote "Pretty BinOp" :: BinOp -> Expression)

instance Pretty UniOp where
  pretty = pretty . (toConjureNote "Pretty UniOP" :: UniOp -> Expression)

instance Pretty Proc where
  pretty = pretty . (toConjureNote "Pretty UniOP" :: Proc -> Expression)

instance Pretty QType where
    pretty = error "Pretty Qtype"
