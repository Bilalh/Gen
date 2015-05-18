{-# LANGUAGE MultiParamTypeClasses #-}
module Gen.Essence.Id where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Expression.Op
import Data.Data
import Gen.Essence.St
import Gen.Imports
import qualified Data.Map as M

instance Pretty (Tree Key) where
    pretty = pretty . displayTree

class (Data a, Pretty a ) => GetKey a where
  getKey  :: a -> Key
  keyTree :: a -> Tree Key

instance GetKey Spec where
  getKey x = fromString . show . toConstr $ x

  keyTree d@(Spec doms exprs obj) =
    Tree (getKey d) $
             [ Tree K_SDoms (map (keyTree . domOfGF ) $ M.elems doms )
             , Tree K_SExprs (map keyTree exprs)
             ] ++ maybeToList (fmap (Tree K_SObj . (: []) . keyTree) obj)

    where

instance GetKey (OObjective, Expr) where
  getKey (o,_)     = fromString . show . toConstr $ o
  keyTree d@(_,e) = Tree (getKey d) [keyTree e]


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

-- FIXME id of op
instance (GetKey a, ExpressionLike a) => GetKey (Op a)


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

  keyTree d@(AbsLitTuple x)                = Tree (getKey d) (map keyTree x)
  keyTree d@(AbsLitRecord x)               = Tree (getKey d) (map (keyTree . snd) x)
  keyTree d@(AbsLitVariant Nothing _ x3)   = Tree (getKey d) [keyTree x3]
  keyTree d@(AbsLitVariant (Just x1) _ x3) = Tree (getKey d) (keyTree x3 :
                                                              map (keyTree . snd) x1)
  keyTree d@(AbsLitMatrix x1 x2)           = Tree (getKey d) (keyTree x1 :
                                                              map keyTree x2)
  keyTree d@(AbsLitSet x)                  = Tree (getKey d) (map keyTree x)
  keyTree d@(AbsLitMSet x)                 = Tree (getKey d) (map keyTree x)
  keyTree d@(AbsLitFunction x)             = Tree (getKey d) (concatMap kt x)
                                             where kt (a,b) = map keyTree [a,b]
  keyTree d@(AbsLitSequence x)             = Tree (getKey d) (map keyTree x)
  keyTree d@(AbsLitRelation x)             = Tree (getKey d) (map keyTree (concat x))
  keyTree d@(AbsLitPartition x)            = Tree (getKey d) (map keyTree (concat x))




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

-- TODO finish
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
