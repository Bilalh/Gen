{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Helpers.Standardise(Standardise(..)) where

import Conjure.Language.Constant
import Conjure.Language.Expression.Op.Internal.Generated
import Gen.Imports

import qualified Data.Traversable as V


class (Pretty a, Eq a, Show a) => Standardise a  where
    standardise :: (Monad m, Applicative m) => a -> m a

instance Standardise Text where
    standardise = return

instance Standardise Type where
    standardise = return

instance Standardise Expr where
    standardise (ECon y)      = return $ ECon y
    standardise (ELit y)      = pure ELit   <*> standardise y
    standardise (EDom y)      = pure EDom   <*> standardise y
    standardise (EOp y)       = pure EOp    <*> standardise y
    standardise (ETyped x y ) = pure ETyped <*> standardise x <*> standardise y

    standardise (EQuan y1 y2 y3 y4 y5) = pure EQuan
                       <*> standardise y1
                       <*> return y2
                       <*> standardise y3
                       <*> standardise y4
                       <*> standardise y5

    standardise x@(EVar _)    = return x
    standardise x@EEmptyGuard = return x
    standardise x = return x


instance Standardise QType where
    standardise = return

instance Standardise Constant where
    standardise = return

instance Standardise Literal where
    standardise x = return x

instance Standardise (Domainn Expr) where
    standardise x = return x

-- FIXME standardise EOP
instance Standardise (Op Expr) where
    standardise x = return x


instance Standardise GF where
    standardise (Findd x)  = pure Findd  <*> standardise x
    standardise (Givenn x) = pure Givenn <*> standardise x

instance Standardise Domains where
    standardise = V.traverse standardise

instance Standardise Spec where
    standardise (Spec x1 x2 x3) = pure Spec <*> standardise x1
                                            <*> mapM standardise x2
                                            <*> standardise x3

instance Standardise OObjective where
    standardise x = return x


instance (Standardise a, Standardise b) =>  Standardise (a,b) where
    standardise (a,b) = do
      sa <- standardise a
      sb <- standardise b
      return (sa,sb)

instance (Standardise a, Standardise b, Standardise c)
    =>  Standardise (a,b,c) where
    standardise (a,b,c) = do
      sa <- standardise a
      sb <- standardise b
      sc <- standardise c
      return (sa,sb, sc)

instance Standardise a => Standardise (Maybe a) where
    standardise Nothing  = pure Nothing
    standardise (Just a) = pure Just <*> standardise a
