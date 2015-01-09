{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module TestGen.Helpers.TypeOf(WithDoms(..), TypeOf(..), typeOfDom) where

import TestGen.Helpers.StandardImports
-- import TestGen.Arbitrary.Data

import qualified Data.Map as M


class (Monad a, Applicative a) => WithDoms a where
  domainOfVar :: Text -> a (Maybe Domain)
  getSpecEWithDoms :: a SpecE
  typeOfVar :: Text -> a (Maybe Type)

  domainOfVar t = do
    (SpecE ds _) <- getSpecEWithDoms
    let d =  fmap domOfFG $ t `M.lookup` ds
    return d

  typeOfVar  t = do
    domainOfVar  t >>= \case
      Nothing -> return Nothing
      Just d  -> ttypeOf d >>= return . Just



class WithDoms m => TypeOf a m where
    ttypeOf :: a -> m Type

instance WithDoms m => TypeOf Type m where
  ttypeOf t = return t

instance WithDoms m => TypeOf Domain m where
  ttypeOf = return . typeOfDom


instance WithDoms m => TypeOf Expr m where
  ttypeOf (ELit x)          = ttypeOf x
  ttypeOf (EDom x)          = ttypeOf x
  ttypeOf (EBinOp x)        = ttypeOf x
  ttypeOf (EUniOp x)        = ttypeOf x
  ttypeOf (EProc x)         = ttypeOf x
  ttypeOf (EQuan Sum _ _ _) = return TInt
  ttypeOf (EQuan _ _ _ _)   = return TBool
  ttypeOf EEmptyGuard       = return TBool

  ttypeOf (EQVar x) = typeOfVar x >>= \case
    Nothing -> error . show . vcat $ ["ttypeOf EVar no domain", pretty x  ]
    Just ty -> return ty

  ttypeOf (EVar x) = typeOfVar x >>= \case
    Nothing -> error . show . vcat $ ["ttypeOf EVar no domain", pretty x  ]
    Just ty -> return ty

  ttypeOf (ETyped t _)  = return t

instance WithDoms m => TypeOf UniOp m where
  ttypeOf (UBar e) = ttypeOf e
  ttypeOf (UNeg e) = ttypeOf e


instance WithDoms m => TypeOf BinOp m where
  ttypeOf (BIn _ _)    = return TBool
  ttypeOf (BOver _ _) = error "withDoms Bover missing"
  ttypeOf (BEQ _ _)    = return TBool
  ttypeOf (BNEQ _ _)   = return TBool
  ttypeOf (BLT _ _)    = return TBool
  ttypeOf (BLTE _ _)   = return TBool
  ttypeOf (BGT _ _)    = return TBool
  ttypeOf (BGTE _ _)   = return TBool

  ttypeOf (BDiff x1 _) = ttypeOf x1
  ttypeOf (BPlus x1 _) = ttypeOf x1
  ttypeOf (BMult x1 _) = ttypeOf x1
  ttypeOf (BDiv x1 _)  = ttypeOf x1
  ttypeOf (BPow x1 _)  = ttypeOf x1
  ttypeOf (BMod x1 _)  = ttypeOf x1

  ttypeOf (BAnd _ _)        = return TBool
  ttypeOf (BOr _ _)         = return TBool
  ttypeOf (Bimply _ _)      = return TBool
  ttypeOf (Biff x1 _)       = ttypeOf x1
  ttypeOf (Bsubset x1 _)    = ttypeOf x1
  ttypeOf (BsubsetEq x1 _)  = ttypeOf x1
  ttypeOf (Bsupset x1 _)    = ttypeOf x1
  ttypeOf (BsupsetEq x1 _)  = ttypeOf x1
  ttypeOf (Bintersect x1 _) = ttypeOf x1
  ttypeOf (Bunion x1 _)     = ttypeOf x1

  ttypeOf (BlexLT _ _)  = return TBool
  ttypeOf (BlexLTE _ _) = return TBool
  ttypeOf (BlexGT _ _)  = return TBool
  ttypeOf (BlexGTE _ _) = return TBool


instance WithDoms m => TypeOf Literal m where
  ttypeOf (EB _) = return TBool
  ttypeOf (EI _) = return TInt

  ttypeOf (ETuple x)        = return TTuple <*> (mapM ttypeOf x)

  ttypeOf (EMatrix [] _)    = return $ TMatix TAny
  ttypeOf (EMatrix (x:_) _) = pure TMatix <*> ttypeOf x
  ttypeOf (ESet [])         = return $ TSet TAny
  ttypeOf (ESet (x:_))      = pure TSet <*> ttypeOf x
  ttypeOf (EMSet [])        = return $ TMSet TAny
  ttypeOf (EMSet (x:_))     = pure TMSet <*> ttypeOf x

  ttypeOf (EFunction [])            = return $ TFunc TAny TAny
  ttypeOf (EFunction ( (x1,x2) :_)) = pure TFunc <*> (ttypeOf x1) <*> (ttypeOf x2)

  ttypeOf (ERelation xs)            = pure  TRel <*> (mapM ( ttypeOf) xs)
  ttypeOf (EPartition [])           = return $ TPar TAny

  ttypeOf (EPartition xs)           = pure TPar <*> (fmap firstOrAny . fmap catMaybes $ toType xs)

    where
      toType :: [[Literal]] -> m [Maybe Type]
      toType ts = case filter (not . null) ts of
                   []    -> return []
                   (y:_) -> mapM (fmap ridAny . ttypeOf) y



      ridAny TAny = Nothing
      ridAny v    = Just v

      firstOrAny :: [Type] -> Type
      firstOrAny []    = TAny
      firstOrAny (x:_) = x

  ttypeOf (EExpr x) = ttypeOf x

instance WithDoms m => TypeOf Proc m where
  ttypeOf (PallDiff x)  = ttypeOf x
  ttypeOf (Pindex _ x2) = ttypeOf x2
  ttypeOf (Papply x1 _) = ttypeOf x1 >>= \case
    TFunc _ b -> return b
    other -> error . show . vcat $ ["Papply type is not TFunc ", pretty other]


  ttypeOf (Pfreq _ _)       = return TInt
  ttypeOf (Phist _ _)       = return $ TMatix TInt
  ttypeOf (Pmax x)          = ttypeOf x
  ttypeOf (Pmin x)          = ttypeOf x
  ttypeOf (PtoInt _)        = return TInt
  ttypeOf (Ptogether _ _ _) = return TBool
  ttypeOf (Pinverse _ _)    = return TBool
  ttypeOf (Papart _ _ _)    = return TBool

  ttypeOf (PtoRelation x) = do
    TFunc a b <- ttypeOf x
    return $ TRel [a,b]

  ttypeOf (PtoMSet x) = pure TMSet <*> (fmap innerTyForSets . ttypeOf) x
  ttypeOf (PtoSet x)  = pure TSet  <*> (fmap innerTyForSets . ttypeOf) x

  ttypeOf (Pdefined x1 ) = do
    TFunc t1 _ <- ttypeOf x1
    return $ TSet t1

  ttypeOf (Pimage x1 _) = do
    TFunc _ b <- ttypeOf x1
    return b

  ttypeOf (PpreImage x1 _) = do
    TFunc a _ <- ttypeOf x1
    return a

  ttypeOf (Prange x) = do
    TFunc _ b <- ttypeOf x
    return b

  ttypeOf (Pparts x) = do
    TPar inn <- ttypeOf x
    return $ TSet $ TSet $ inn

  ttypeOf (Pparty _ x2) = do
    TPar inn <- ttypeOf x2
    return $ TSet inn

  ttypeOf (Pparticipants x) = do
    TPar inn <- ttypeOf x
    return $ TSet inn


innerTyForSets :: Type -> Type
innerTyForSets  (TSet ty)   = ty
innerTyForSets  (TMSet ty)  = ty
innerTyForSets  (TRel xs)   = TTuple xs
innerTyForSets  (TFunc a b) = TTuple [a,b]
innerTyForSets ty = error . show . vcat $ ["innerTyForSets other type given ", pretty ty]


typeOfDom :: Domain -> Type
typeOfDom  DInt{} = TInt
typeOfDom  DBool  = TBool

typeOfDom DMat{inner}  = TMatix (typeOfDom inner)
typeOfDom DSet{inner}  = TSet   (typeOfDom inner)
typeOfDom DMSet{inner} = TMSet  (typeOfDom inner)
typeOfDom DPar{inner}  = TPar  (typeOfDom inner)

typeOfDom DRel{inners} = TRel (map typeOfDom inners)
typeOfDom DFunc{innerFrom,innerTo} =
    TFunc (typeOfDom innerFrom) (typeOfDom innerTo)

typeOfDom DTuple{inners} = TTuple (map typeOfDom inners)
