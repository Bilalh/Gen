module TestGen.Helpers.TypeOf where

import TestGen.Prelude
import TestGen.Arbitrary.Type(typeOfDom)

class TypeOf a where
    ttypeOf :: a -> Type


instance TypeOf Domain where
  ttypeOf  = typeOfDom


instance TypeOf Expr where
  ttypeOf (ELit x) = ttypeOf x
  ttypeOf (EDom x) = ttypeOf x
  -- ttypeOf (EVar x) = 
  -- ttypeOf (EQVar x) = _ttypeOf_body
  ttypeOf (EBinOp x) = ttypeOf x
  -- ttypeOf (EUniOp x) = _ttypeOf_body
  -- ttypeOf (EProc x) = _ttypeOf_body
  ttypeOf (EQuan _ _ _ _) = TBool
  ttypeOf EEmptyGuard = TBool


instance TypeOf BinOp where
  ttypeOf (BIn _ _)       = TBool
  -- ttypeOf (BOver x1 _) = _ttypeOf_body
  ttypeOf (BEQ _ _)       = TBool
  ttypeOf (BNEQ _ _)      = TBool
  ttypeOf (BLT _ _)       = TBool
  ttypeOf (BLTE _ _)      = TBool
  ttypeOf (BGT _ _)       = TBool
  ttypeOf (BGTE _ _)      = TBool

  ttypeOf (BDiff x1 _) = ttypeOf x1
  ttypeOf (BPlus x1 _) = ttypeOf x1
  ttypeOf (BMult x1 _) = ttypeOf x1
  ttypeOf (BDiv x1 _)  = ttypeOf x1
  ttypeOf (BPow x1 _)  = ttypeOf x1
  ttypeOf (BMod x1 _)  = ttypeOf x1

  ttypeOf (BAnd _ _)        = TBool
  ttypeOf (BOr _ _)         = TBool
  ttypeOf (Bimply _ _)      = TBool
  ttypeOf (Biff x1 _)       =  ttypeOf x1
  ttypeOf (Bsubset x1 _)    =  ttypeOf x1
  ttypeOf (BsubsetEq x1 _)  =  ttypeOf x1
  ttypeOf (Bsupset x1 _)    =  ttypeOf x1
  ttypeOf (BsupsetEq x1 _)  =  ttypeOf x1
  ttypeOf (Bintersect x1 _) =  ttypeOf x1
  ttypeOf (Bunion x1 _)     =  ttypeOf x1
  
  ttypeOf (BlexLT _ _)  = TBool
  ttypeOf (BlexLTE _ _) = TBool
  ttypeOf (BlexGT _ _)  = TBool
  ttypeOf (BlexGTE _ _) = TBool


instance TypeOf Literal where
  ttypeOf (EB _) = TBool
  ttypeOf (EI _) = TInt
  
  ttypeOf (ETuple x)        = TTuple (map ttypeOf x)

  ttypeOf (EMatrix [] _)    = TMatix TAny
  ttypeOf (EMatrix (x:_) _) = TMatix (ttypeOf x)
  ttypeOf (ESet [])         = TSet TAny
  ttypeOf (ESet (x:_))      = TSet (ttypeOf x)
  ttypeOf (EMSet [])        = TAny
  ttypeOf (EMSet (x:_))     = TMSet (ttypeOf x)

  ttypeOf (EFunction [])            = TFunc TAny TAny
  ttypeOf (EFunction ( (x1,x2) :_)) = TFunc (ttypeOf x1) (ttypeOf x2)

  ttypeOf (ERelation xs)            = TRel (map ( ttypeOf) xs)
  ttypeOf (EPartition [])           = TPar TAny
  
  ttypeOf (EPartition xs)           = TPar (firstOrAny . catMaybes $ toType xs)
  
    where
      toType :: [[Literal]] -> [Maybe Type]
      toType ts = case filter (not . null) ts of
                [] -> []
                (y:_) -> map (ridAny .  ttypeOf) y

      ridAny TAny = Nothing
      ridAny v    = Just v

      firstOrAny :: [Type] -> Type
      firstOrAny []    = TAny
      firstOrAny (x:_) = x
  
  ttypeOf (EExpr x) = ttypeOf x

