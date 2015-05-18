{-# LANGUAGE MultiParamTypeClasses #-}
module Gen.Essence.Id where

import Gen.Essence.St
import Gen.Imports
import Data.Data
import Conjure.Language.Definition
import Conjure.Language.Domain


class (Data a, Pretty a ) => GetKey a where
  getKey  :: a -> Key
  keyTree :: a -> Tree Key

instance GetKey Type where
  getKey x   = fromString . show . toConstr $ x
  keyTree d  =  Tree (getKey d) (map keyTree $ children d)

instance GetKey Expr where
  getKey x = fromString . show . toConstr $ x

  keyTree d@(EVar _)               = Tree (getKey d) []
  keyTree d@(ECon x)               = Tree (getKey d) [keyTree x]
  keyTree d@(ELit x)               = Tree (getKey d) [keyTree x]
  keyTree d@(EOp x)                = Tree (getKey d) []
  keyTree d@(ETyped x1 x2)         = Tree (getKey d) [keyTree x1, keyTree x2]
  keyTree d@(EDom x)               = Tree (getKey d) [keyTree x]

  -- keyTree d@(EComp x1 x2 x3)       = Tree (getKey d) []
  keyTree d = docError ["unhandled " <+> pretty d <+> "in buildTree" ]

instance GetKey Constant where
  getKey x = fromString . show . toConstr $ x

  keyTree d@(ConstantAbstract x) = Tree (getKey d) [keyTree x]
  keyTree d@(ConstantInt x)      = Tree (getKey d) [keyTree x]
  keyTree d                      = Tree (getKey d) []


instance GetKey Integer where
  getKey x  = fromString $ "Int_" ++ show x
  keyTree x = Tree (getKey x) []


instance GetKey a => GetKey (AbstractLiteral a) where
  getKey x = fromString . show . toConstr $ x
  keyTree x@(AbsLitSet xs) = Tree (getKey x) (map keyTree xs)


instance GetKey a => GetKey (Range a) where
  getKey x = fromString . show . toConstr $ x
  keyTree d@RangeOpen             = Tree (getKey d) ([])
  keyTree d@(RangeSingle x)       = Tree (getKey d) ([keyTree x])
  keyTree d@(RangeLowerBounded x) = Tree (getKey d) ([keyTree x])
  keyTree d@(RangeUpperBounded x) = Tree (getKey d) ([keyTree x])
  keyTree d@(RangeBounded x1 x2)  = Tree (getKey d) ([keyTree x1, keyTree x2])


instance GetKey a => GetKey (Domain () a) where
  getKey x = fromString . show . toConstr $ x

  keyTree d@DomainBool                  = Tree (getKey d) ([])
  keyTree d@DomainIntEmpty              = Tree (getKey d) ([])
  keyTree d@(DomainInt x)               = Tree (getKey d) (map keyTree x)
  keyTree d@(DomainEnum _ _ _)          = Tree (getKey d) ([])
  keyTree d@(DomainUnnamed _ _)         = Tree (getKey d) ([])
  keyTree d@(DomainTuple x)             = Tree (getKey d) (map keyTree x)
  keyTree d@(DomainRecord x)            = Tree (getKey d) (map (keyTree . snd) x)
  keyTree d@(DomainVariant x)           = Tree (getKey d) (map (keyTree . snd) x)
  keyTree d@(DomainMatrix x1 x2)        = Tree (getKey d) (map keyTree [x1,x2])
  keyTree d@(DomainSet _ x2 x3)         = Tree (getKey d) [keyTree x2, keyTree x3]
  keyTree d@(DomainMSet _ x2 x3)        = Tree (getKey d) [keyTree x2, keyTree x3]
  keyTree d@(DomainFunction _ x2 x3 x4) = Tree (getKey d) [keyTree x2, keyTree x3,keyTree x4]
  keyTree d@(DomainSequence _ x2 x3)    = Tree (getKey d) [keyTree x2, keyTree x3]
  keyTree d@(DomainRelation _ x2 x3)    = Tree (getKey d) (keyTree x2 : map keyTree x3)
  keyTree d@(DomainPartition _ x2 x3)   = Tree (getKey d) [keyTree x2, keyTree x3]

  keyTree d = docError ["unhandled " <+> pretty d <+> "in buildTree" ]

instance GetKey a => GetKey (FunctionAttr a) where
  getKey x  = fromString . show . toConstr $ x
  keyTree d = Tree (getKey d) ([])

instance GetKey a => GetKey (MSetAttr a) where
  getKey x  = fromString . show . toConstr $ x
  keyTree d = Tree (getKey d) ([])

instance GetKey a => GetKey (PartitionAttr a) where
  getKey x  = fromString . show . toConstr $ x
  keyTree d = Tree (getKey d) ([])

instance GetKey a => GetKey (RelationAttr a) where
  getKey x  = fromString . show . toConstr $ x
  keyTree d = Tree (getKey d) ([])

instance GetKey a => GetKey (SequenceAttr a) where
  getKey x  = fromString . show . toConstr $ x
  keyTree d = Tree (getKey d) ([])

instance GetKey a => GetKey (SetAttr a) where
  getKey x  = fromString . show . toConstr $ x
  keyTree d = Tree (getKey d) ([])


-- | 2d drawing of the tree horizontally
displayTree :: Show a => Tree a -> String
displayTree  = unlines . draw2
  where
  draw2 :: Show a => Tree a -> [String]
  draw2 (Tree x ts0) = show x : drawSubTrees ts0
    where
      drawSubTrees [] = []
      drawSubTrees [t] =
          "|" : shift "`- " "   " (draw2 t)
      drawSubTrees (t:ts) =
          "|" : shift "+- " "|  " (draw2 t) ++ drawSubTrees ts

      shift fr other = zipWith (++) (fr : repeat other)
