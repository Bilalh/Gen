{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen.Helpers.TypeOf(WithDoms(..),TTypeOf(..), typeOfDom) where

import Conjure.Language.AdHoc         ((:<)(..) )
import Conjure.Language.Constant
import Conjure.Language.Domain
import Conjure.Language.Expression.Op
import Conjure.Language.TypeOf
import Gen.Imports

import qualified Data.Map as M


class TTypeOf a where
    ttypeOf :: (Monad m, Applicative m) => a -> m Type

instance TTypeOf Type  where
  ttypeOf t = return t

instance TTypeOf Var  where
  ttypeOf (Var _ ty )= return ty

instance TTypeOf GF  where
  ttypeOf = return . typeOfDom . domOfGF

instance TTypeOf (Domainn Expr)  where
  ttypeOf = return . typeOfDom

instance TTypeOf Constant  where
    ttypeOf x = toTType x

instance (TTypeOf x, Pretty x, TypeOf x) => TTypeOf (AbstractLiteral x)  where
    ttypeOf x = toTType x

instance TTypeOf (Op Expr) where
    ttypeOf x = toTType x

instance TTypeOf Expr  where
  ttypeOf (ELit x)            = ttypeOf x
  ttypeOf (ECon x)            = ttypeOf x
  ttypeOf (EDom x)            = ttypeOf x
  ttypeOf (EQuan Sum _ _ _ _) = return TypeInt
  ttypeOf (EQuan _ _ _ _ _)   = return TypeBool
  ttypeOf EEmptyGuard         = return TypeBool
  ttypeOf (EVar v)            = ttypeOf v
  ttypeOf (ETyped t _)        = return t
  ttypeOf (EOp op)            = ttypeOf op
  -- TODO could also be a set
  ttypeOf (EComp inner _ _)   = (TypeMatrix TypeInt) <$> ttypeOf inner

  ttypeOf x = error . show . vcat $ ["ttypeOf failed for "
                                    , pretty x
                                    , pretty . groom $ x]




typeOfDom :: (Domainn Expr) ->Type
typeOfDom d = case typeOf d of
                Left x -> error . show . vcat $
                          ["typeOfDom failed for", x, (pretty . groom) d, pretty d]
                Right x -> x


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

toTType :: (Monad m, Applicative m, TypeOf a, Show a) => a -> m Type
toTType f = case typeOf f of
              Left r   -> error . show . vcat  $ ["toTType error"
                                                 , r
                                                 , pretty . groom $ f ]
              Right r  -> return  r


instance TypeOf Expr  where
  typeOf (EVar (Var _ ty )) = return ty
  typeOf t = do
      ty ::Type <- ttypeOf t
      return ty

instance Domain () Expr :< Expr where
    inject = EDom
    project (EDom x) = return x
    project x = fail ("projecting Domain out of Expr:" <+> pretty x)
