{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module TestGen.Helpers.TypeOf where

import TestGen.Prelude
import TestGen.Arbitrary.Type(typeOfDom)

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
         

instance WithDoms (StateT SpecE Identity) where
  getSpecEWithDoms = get


class WithDoms m => TypeOf a m where
    ttypeOf :: a -> m Type

 
instance WithDoms m => TypeOf Domain m where
  ttypeOf = return . typeOfDom


instance WithDoms m => TypeOf Expr m where
  ttypeOf (ELit x) = ttypeOf x
  ttypeOf (EDom x) = ttypeOf x
  -- ttypeOf (EVar x) = 
  -- ttypeOf (EQVar x) = _ttypeOf_body
  ttypeOf (EBinOp x) = ttypeOf x
  -- ttypeOf (EUniOp x) = _ttypeOf_body
  -- ttypeOf (EProc x) = _ttypeOf_body
  ttypeOf (EQuan _ _ _ _) = return TBool
  ttypeOf EEmptyGuard = return TBool


instance WithDoms m => TypeOf BinOp m where
  ttypeOf (BIn _ _)       = return TBool
  ttypeOf (BOver x1 _)    = return undefined
  ttypeOf (BEQ _ _)       = return TBool
  ttypeOf (BNEQ _ _)      = return TBool
  ttypeOf (BLT _ _)       = return TBool
  ttypeOf (BLTE _ _)      = return TBool
  ttypeOf (BGT _ _)       = return TBool
  ttypeOf (BGTE _ _)      = return TBool

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
  ttypeOf (EMSet [])        = return $ TAny
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
