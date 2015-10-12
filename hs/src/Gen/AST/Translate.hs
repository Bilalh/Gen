{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.AST.Translate where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Expression.Op
import Conjure.Language.Pretty
import Conjure.Language.TH
import Conjure.Language.TypeOf
import Conjure.Prelude
import Gen.AST.Data
import Gen.Helpers.LineError
import Text.PrettyPrint as Pr

instance Translate Constant Constant where
    fromConjure = return . id
    toConjure   = return . id

instance Translate  (AbstractLiteral Expr) (AbstractLiteral Expression) where
    fromConjure x = mapM fromConjure x
    toConjure x   = mapM toConjure x

instance Translate (Op Expr) (Op Expression) where
    fromConjure x =  mapM fromConjure x
    toConjure x   =  fixRelationProj <$> mapM toConjure x

instance Translate (Domain () Expr) (Domain () Expression) where
    fromConjure x = mapM fromConjure x
    toConjure x   = mapM toConjure x

instance Translate (EGen) (Generator) where
    fromConjure (GenDomainNoRepr x1 x2)  = GenDom <$> return x1 <*> fromConjure x2
    fromConjure (GenInExpr x1 x2)        = GenIn  <$> return x1 <*> fromConjure x2

    fromConjure x = fromConjureFail "Translate (EGen) (Generator)" x

    toConjure (GenDom x1 x2) = GenDomainNoRepr <$> return x1 <*> toConjure x2
    toConjure (GenIn x1 x2)  = GenInExpr       <$> return x1 <*> toConjure x2

instance Translate Expr Expression where
  fromConjure (Constant t)          = ECon   <$> fromConjure t
  fromConjure (AbstractLiteral t)   = ELit   <$> fromConjure t
  fromConjure (Domain t)            = EDom   <$> fromConjure t
  fromConjure (Typed t1 t2)         = ETyped <$> pure t2 <*> fromConjure t1
  fromConjure (Op op)               = EOp    <$> fromConjure op
  fromConjure (ExpressionMetaVar t) = return $ EMetaVar t

  -- fromConjure (WithLocals t1 t2)    = _f
  fromConjure r@(Reference t1 _)         = do
    name <- fromConjure t1
    ty   <- typeOf r
    return . EVar $ Var name ty

  fromConjure (Comprehension inner genCon) = EComp <$> fromConjure inner
                                                   <*> mapM fromConjure gens
                                                   <*> mapM fromConjure cons
    where
      gens = [ g | Generator g <- genCon ]
      cons = [ c | Condition c <- genCon ]



  fromConjure x = fromConjureFail "Expr Expression" x

  toConjure (ECon x )      =  return $ Constant x
  toConjure (ELit x )      =  AbstractLiteral <$> toConjure x
  toConjure (EDom x)       =  Domain <$> toConjure x
  toConjure (ETyped x1 x2) =  Typed <$> toConjure x2 <*> pure x1
  toConjure (EOp x)        =  Op <$> toConjure x

  toConjure (EVar (Var x _) ) =  Reference <$> toConjure x <*> return Nothing
  toConjure (EMetaVar x)      = return $ ExpressionMetaVar x

  toConjure (EComp inner gens cons) =
                    Comprehension
                <$> toConjure inner
                <*> (
                     (++) <$> (mapM ( fmap  Generator . toConjure) gens)
                          <*> (mapM ( fmap  Condition . toConjure) cons)
                    )



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


  toConjure x = toConjureFail "Expr Expression" x


instance (Translate a b) => Translate (a,a) (b,b) where
    toConjure (x,y) = do
      xx <- toConjure x
      yy <- toConjure y
      return (xx, yy)

    fromConjure (x,y) = do
      xx <- fromConjure x
      yy <- fromConjure y
      return (xx, yy)


instance Pretty Expr where
  pretty = pretty . (toConjureNote "Pretty Expr" :: Expr -> Expression)

instance Pretty QType where
    pretty ForAll = "forAll"
    pretty Exists = "exists"
    pretty Sum    = "sim"

instance Pretty EGen where
    pretty =  pretty . (toConjureNote "Pretty EGen" :: EGen -> Generator)


instance Translate Text Name where
    toConjure x          = pure $ Name x
    fromConjure (Name x) = pure x
    fromConjure x        = notHandled $line "Translate Text Name" x


-- Conjure required instances

instance ExpressionLike Expr where
    fromInt = ECon . fromInt
    intOut doc (ECon c) = intOut doc c
    intOut doc x = fail ("Expecting a constant, but got:" <+> pretty x <+> "from" <+> doc)

    fromBool = ECon . fromBool
    boolOut (ECon c) = boolOut c
    boolOut x = fail ("Expecting a constant, but got:" <+> pretty x)

    fromList xs = ELit $ AbsLitMatrix (dintRange 1 (genericLength xs)) xs
    listOut (ELit (AbsLitMatrix _ xs)) = return xs
    listOut (ECon (ConstantAbstract (AbsLitMatrix _ xs))) = return (map ECon xs)
    listOut c = fail ("Expecting a matrix literal, but found:" <+> pretty c)


instance ReferenceContainer Expr where
    fromName (Name t)                  = EVar $ Var t (error $ "no type for fromName " ++ show t)
    fromName m@MachineName{}           = EVar $ Var (stringToText $ show $ pretty $ m) (error $ "no type for fromName " ++ show m)

    nameOut (EVar (Var nm _))           = return $ Name nm
    nameOut (ECon (ConstantField nm _)) = return $  nm
    nameOut p = fail ("This expression isn't a 'name':" <+> pretty p)


-- functions

dintRange :: Int -> Int -> Domain () Expr
dintRange a b = DomainInt [RangeBounded (ECon . ConstantInt $ fromIntegral a)
                                        (ECon . ConstantInt $ fromIntegral b)]

cintRange :: Int -> Int -> Domain () Constant
cintRange a b = DomainInt [RangeBounded ( ConstantInt $ fromIntegral a)
                                        ( ConstantInt $ fromIntegral b)]




instance PrettyWithQuan Expr where
    prettyWithQuan (EQuan qtype (Var name _) over_in guar body) =
      let
        header =  pretty qtype
              <+> pretty name
              <+> ( case over_in of
                        (EDom dom) -> Pr.colon <+> pretty dom
                        x          -> "in" <+> prettyWithQuan x
                  )
        hangGuard x = case guar of
                        EEmptyGuard -> x
                        _ -> x
        hangBody x = x <++> ("." <+> prettyWithQuan body)
      in hangBody $ hangGuard header

    prettyWithQuan x = pretty x
