{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen.Helpers.TypeOf(WithDoms(..), TTypeOf(..), typeOfDom) where

import Conjure.Language.Constant
import Conjure.Language.TypeOf
import Conjure.Language.Expression.Op
import Conjure.Language.Definition
import Gen.Helpers.StandardImports

import qualified Data.Map as M


class TTypeOf a where
    ttypeOf :: (Monad m, Applicative m) => a -> m TType

instance TTypeOf TType  where
  ttypeOf t = return t

instance TTypeOf Var  where
  ttypeOf (Var _ ty )= return ty

instance TTypeOf GF  where
  ttypeOf = return . typeOfDom . domOfGF

instance TTypeOf (Domainn Expr)  where
  ttypeOf = return . typeOfDom

instance TTypeOf Constant  where
    ttypeOf x = toTType x

instance TTypeOf Literal  where
    ttypeOf x = toTType x

instance TTypeOf (Op Expr) where
    ttypeOf x = do
      let cx :: Op Expression = toConjureNote "TTypeOf (Op Expr)" x
      toTType cx

instance TTypeOf Expr  where
  ttypeOf (ELit x)            = ttypeOf x
  ttypeOf (ECon x)            = ttypeOf x
  ttypeOf (EDom x)            = ttypeOf x
  ttypeOf (EQuan Sum _ _ _ _) = return TInt
  ttypeOf (EQuan _ _ _ _ _)   = return TBool
  ttypeOf EEmptyGuard         = return TBool
  ttypeOf (EVar v)            = ttypeOf v
  ttypeOf (ETyped t _)        = return t
  ttypeOf (EOp op)            = ttypeOf op
  ttypeOf x = error . show . vcat $ ["ttypeOf ", pretty x]




typeOfDom :: (Domainn Expr) -> TType
typeOfDom d = case typeOf d of
                Left x -> error . show . vcat $
                          ["typeOfDom failed for", x, (pretty . groom) d, pretty d]
                Right x -> fromConjureNote "typeOfDom convert type back" x


class (Monad a, Applicative a) => WithDoms a where
  domainOfVar :: Text -> a (Maybe (Domainn Expr))
  getSpecEWithDoms :: a Spec

  domainOfVar t = do
    (Spec ds _ _) <- getSpecEWithDoms
    let d =  fmap domOfGF $ t `M.lookup` ds
    return d



instance WithDoms ((->) Spec) where
    getSpecEWithDoms e = e

instance WithDoms m => WithDoms (StateT () m)  where
    getSpecEWithDoms = getSpecEWithDoms

-- Conjure TypeOf stuff


toTType :: (Monad m, Applicative m, TypeOf a, Show a) => a -> m TType
toTType f = case typeOf f of
              Left r   -> error . show . vcat  $ ["toTType error"
                                                 , r
                                                 , pretty . groom $ f ]
              Right r  -> return $ fromConjureNote "toTType"  r


instance TypeOf TType  where
  typeOf t = return $ toConjureNote "typeOf TType" t

instance TypeOf Expr  where
  typeOf t = do
      ty <- (ttypeOf t)
      return $ toConjureNote "typeOf TType" ty
