{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen.Helpers.TypeOf(WithDoms(..), TTypeOf(..), typeOfDom) where

import Conjure.Language.Constant
import Conjure.Language.TypeOf
import Gen.Helpers.StandardImports

import qualified Data.Map as M


class TTypeOf a where
    ttypeOf :: (Monad m, Applicative m) => a -> m TType

instance TTypeOf TType  where
  ttypeOf t = return t

instance TypeOf TType  where
  typeOf t = return $ toConjureNote "typeOf TType" t

instance TypeOf Expr  where
  typeOf t = do
      ty <- (ttypeOf t)
      return $ toConjureNote "typeOf TType" ty

instance TTypeOf GF  where
  ttypeOf = return . typeOfDom . domOfGF

instance TTypeOf (Domainn Expr)  where
  ttypeOf = return . typeOfDom

instance TTypeOf Var  where
  ttypeOf (Var _ ty )= return ty

instance TTypeOf Constant  where
    ttypeOf x = case typeOf x of
                  Left d -> error . show $ d
                  Right r -> return $ fromConjureNote "d" r

instance TTypeOf Expr  where
  ttypeOf (ELit x)          = ttypeOf x
  ttypeOf (ECon x)          = ttypeOf x
  ttypeOf (EDom x)          = ttypeOf x
  ttypeOf (EBinOp x)        = ttypeOf x
  ttypeOf (EUniOp x)        = ttypeOf x
  ttypeOf (EProc x)         = ttypeOf x
  ttypeOf (EQuan Sum _ _ _) = return TInt
  ttypeOf (EQuan _ _ _ _)   = return TBool
  ttypeOf EEmptyGuard       = return TBool
  ttypeOf (EVar v)          = ttypeOf v
  ttypeOf (ETyped t _)      = return t
  ttypeOf x = error . show . vcat $ ["ttypeOf ", pretty x]


toTType :: (Monad m, Applicative m, TypeOf a) => a -> m TType
toTType f = case typeOf f of
              Left r   -> error . show $ r
              Right r  -> return $ fromConjureNote "toTType"  r

instance TTypeOf Literal  where
    ttypeOf x = toTType x


instance TTypeOf UniOp  where
  ttypeOf (UBar e) = ttypeOf e
  ttypeOf (UNeg e) = ttypeOf e


instance TTypeOf BinOp  where
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



instance TTypeOf Proc  where
  ttypeOf (PallDiff x)  = ttypeOf x
  ttypeOf (Pindex _ x2) = ttypeOf x2
  ttypeOf (Papply x1 _) = ttypeOf x1 >>= \case
    TFunc _ b -> return b
    other -> error . show . vcat $ ["Papply type is not TFunc ", pretty . show $ other]


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


innerTyForSets :: TType -> TType
innerTyForSets  (TSet ty)   = ty
innerTyForSets  (TMSet ty)  = ty
innerTyForSets  (TRel xs)   = TTuple xs
innerTyForSets  (TFunc a b) = TTuple [a,b]
innerTyForSets ty = error . show . vcat $ ["innerTyForSets other type given ", pretty . show $  ty]



typeOfDom :: (Domainn Expr) -> TType
typeOfDom d = case typeOf d of
                Left x -> error . show . vcat $
                          ["typeOfDom failed for", x, (pretty . groom) d, pretty d]
                Right x -> fromConjureNote "typeOfDom convert type back" x


class (Monad a, Applicative a) => WithDoms a where
  domainOfVar :: Text -> a (Maybe (Domainn Expr))
  getSpecEWithDoms :: a Spec
  typeOfVar :: Text -> a (Maybe TType)

  domainOfVar t = do
    (Spec ds _ _) <- getSpecEWithDoms
    let d =  fmap domOfGF $ t `M.lookup` ds
    return d

  typeOfVar  t = do
    domainOfVar  t >>= \case
      Nothing -> return Nothing
      Just d  -> ttypeOf d >>= return . Just

instance WithDoms ((->) Spec) where
    getSpecEWithDoms e = e

instance WithDoms m => WithDoms (StateT () m)  where
    getSpecEWithDoms = getSpecEWithDoms
