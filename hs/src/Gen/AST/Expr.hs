{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.AST.Expr where

import Conjure.Language.Definition
import Conjure.Language.Domain        (Domain)
import Conjure.Language.Expression.Op
import Conjure.Language.Pretty
import Conjure.Language.TH
import Conjure.Prelude
import Gen.AST.Data
import Gen.AST.Domain                 (dintRange)
import Gen.AST.Type                   ()
import Text.Groom                     (groom)


fromConjureM :: (Translate a b, MonadFail m) => [b] -> m [a]
fromConjureM as =  mapM fromConjure as

toConjureM :: (Translate a b, MonadFail m) => [a] -> m [b]
toConjureM as =  mapM toConjure as

instance (Translate a b) => Translate (a,a) (b,b) where
    toConjure (x,y) = do
      xx <- toConjure x
      yy <- toConjure y
      return (xx, yy)

    fromConjure (x,y) = do
      xx <- fromConjure x
      yy <- fromConjure y
      return (xx, yy)

instance Translate Constant Constant where
    fromConjure = return . id
    toConjure   = return . id


instance Translate Literal (AbstractLiteral Expression) where
    fromConjure (AbsLitTuple x)             = AbsLitTuple <$> fromConjureM x
    -- fromConjure (AbsLitRecord x)         = AbsLitRecord <$> fromConjureM x
    -- fromConjure (AbsLitVariant x1 x2 x3) = AbsLitVariant <$> fromConjure x1
    --                                                      <*> fromConjure x2
    --                                                      <*> fromConjure x3
    fromConjure (AbsLitMatrix x1 x2)        =  AbsLitMatrix <$> fromConjure x1
                                                         <*> fromConjureM x2
    fromConjure (AbsLitSet x)               = AbsLitSet <$> fromConjureM x
    fromConjure (AbsLitMSet x)              = AbsLitMSet <$> fromConjureM x
    fromConjure (AbsLitFunction x)          = AbsLitFunction <$> fromConjureM x
    -- fromConjure (AbsLitSequence x)       = _d
    -- fromConjure (AbsLitRelation x)       = _d
    -- fromConjure (AbsLitPartition x)      = _d

    fromConjure x = fromConjureFail "Literal Literalession)" x

    toConjure (AbsLitTuple x)             = AbsLitTuple     <$> toConjureM x
    -- toConjure (AbsLitRecord x)         = AbsLitRecord    <$> toConjure x
    -- toConjure (AbsLitVariant x1 x2 x3) = AbsLitVariant   <$> toConjure x1
    --                                                      <*> toConjure x2
    --                                                      <*> toConjure x3
    toConjure (AbsLitMatrix x1 x2)        = AbsLitMatrix    <$> toConjure x1
                                                            <*> toConjureM x2
    toConjure (AbsLitSet x)               = AbsLitSet       <$> toConjureM x
    toConjure (AbsLitMSet x)              = AbsLitMSet      <$> toConjureM x
    toConjure (AbsLitFunction x)          = AbsLitFunction  <$> toConjureM x
    toConjure (AbsLitSequence x)          = AbsLitSequence  <$> toConjureM x
    toConjure (AbsLitRelation x)          = AbsLitRelation  <$> mapM toConjureM x
    toConjure (AbsLitPartition x)         = AbsLitPartition <$> mapM toConjureM x

    toConjure x = toConjureFail "Literal Literalession)" x

instance Translate Expr Expression where
  fromConjure (Constant t)             = ECon   <$> fromConjure t
  fromConjure (AbstractLiteral t)      = ELit   <$> fromConjure t
  fromConjure (Domain t)               = EDom   <$> fromConjure t
  -- fromConjure (WithLocals t1 t2)    = _f
  -- fromConjure (Comprehension t1 t2) = _f
  fromConjure (Typed t1 t2)            = ETyped <$> fromConjure t2 <*> fromConjure t1
  fromConjure (Op op) = EOp <$> mapM fromConjure op

  -- fromConjure (Reference t1 x)         = EVar   <$> fromConjure t1
  fromConjure (ExpressionMetaVar t) = return $ EMetaVar t

  fromConjure x = fromConjureFail "Expr Expression" x


  toConjure (ECon x )      =  return $ Constant x
  toConjure (ELit x )      =  AbstractLiteral <$> toConjure x
  toConjure (EDom x)       =  Domain <$> toConjure x
  toConjure (ETyped x1 x2) =  Typed <$> toConjure x2 <*> toConjure x1
  toConjure (EOp x)        =  Op <$> mapM toConjure x

  --FIXME correct? not the first
  toConjure (EVar (Var x _) ) =  Reference <$> toConjure x <*> return Nothing
  toConjure (EMetaVar x)      = return $ ExpressionMetaVar x


  toConjure (EQuan q (Var x _) dom g inner) = do
        x'     <- return $ Single (Name x)
        dom'   <- toConjure dom
        inner' <- toConjure inner
        case (q,g) of
          (ForAll,EEmptyGuard) -> return [essence| forAll &x' in &dom' . &inner' |]
          (Exists,EEmptyGuard) -> return [essence| exists &x' in &dom' . &inner' |]
          (Sum,EEmptyGuard)    -> return [essence| sum    &x' in &dom' . &inner' |]
          (ForAll,_)           -> toConjure g  >>= \g' ->
                                  return [essence| forAll &x' in &dom', &g' . &inner' |]
          (Exists,_)           -> toConjure g  >>= \g' ->
                                  return [essence| exists &x' in &dom', &g' . &inner' |]
          (Sum,_)              -> toConjure g  >>= \g' ->
                                  return [essence| sum    &x' in &dom', &g' . &inner' |]

  toConjure (EQuan q (Var x _) (EDom dom) g inner) = do
        x'                           <- return $ Single (Name x)
        dom' :: Domain () Expression <- toConjure dom
        inner'                       <- toConjure inner
        case (q,g) of
          (ForAll,EEmptyGuard) -> return [essence| forAll &x' : &dom' . &inner' |]
          (Exists,EEmptyGuard) -> return [essence| exists &x' : &dom' . &inner' |]
          (Sum,EEmptyGuard)    -> return [essence| sum    &x' : &dom' . &inner' |]
          (ForAll,_)           -> toConjure g  >>= \g' ->
                                  return [essence| forAll &x' : &dom', &g' . &inner' |]
          (Exists,_)           -> toConjure g  >>= \g' ->
                                  return [essence| exists &x' : &dom', &g' . &inner' |]
          (Sum,_)              -> toConjure g  >>= \g' ->
                                  return [essence| sum    &x' : &dom', &g' . &inner' |]


  toConjure x = toConjureFail "Expr Expression" x


instance Pretty Expr where
  pretty = pretty . (toConjureNote "Pretty Expr" :: Expr -> Expression)

instance Pretty QType where
    pretty = error "Pretty Qtype"


instance ExpressionLike Expr where
    fromInt = ECon . fromInt
    intOut (ECon c) = intOut c
    intOut x = fail ("Expecting a constant, but got:" <+> pretty x)

    fromBool = ECon . fromBool
    boolOut (ECon c) = boolOut c
    boolOut x = fail ("Expecting a constant, but got:" <+> pretty x)

    fromList xs = ELit $ AbsLitMatrix (dintRange 1 (genericLength xs)) xs
    listOut (ELit (AbsLitMatrix _ xs)) = return xs
    listOut (ECon (ConstantAbstract (AbsLitMatrix _ xs))) = return (map ECon xs)
    listOut c = fail ("Expecting a matrix literal, but found:" <+> pretty c)
