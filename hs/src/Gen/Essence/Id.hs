{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             DeriveTraversable, MultiParamTypeClasses #-}

-- N.B. Imported by Gen.Essence.St
module Gen.Essence.Id( keyList
                     , KTree(..)
                     , GetKey(..)
                     , displayTree
                     , displayKTree
                     ) where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Expression.Op
import Data.Data
import Data.Generics.Uniplate.Data    (childrenBi)
import Gen.Essence.Data.Key
import Gen.Imports

import qualified Data.Foldable as F
import qualified Data.Map      as M
import qualified Data.Set      as S

data KTree a = KTree a  [KTree a]
             | KWeight a Int
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

class (Data a, Pretty a ) => GetKey a where
  getKey  :: a -> Key
  keyTree :: a -> KTree Key

keyList :: GetKey a => a ->  [Key]
keyList = F.toList . keyTree


instance Pretty (KTree Key) where
    pretty = pretty . displayKTree

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

-- | 2d drawing of the tree horizontally
displayKTree :: (Show a) => KTree a -> String
displayKTree  = unlines . draw2
  where
  draw2 :: Show a => KTree a -> [String]
  draw2 (KWeight x i) = [show x, "  W:" ++ show i]
  draw2 (KTree x ts0) = show x : drawSubTrees ts0
    where
      drawSubTrees [] = []
      drawSubTrees [t] =
          "|" : shift "`- " "   " (draw2 t)
      drawSubTrees (t:ts) =
          "|" : shift "+- " "|  " (draw2 t) ++ drawSubTrees ts

      shift fr other = zipWith (++) (fr : repeat other)


instance GetKey Spec where
  getKey x = fromString . show . toConstr $ x

  keyTree d@(Spec doms exprs obj) =
    KTree (getKey d) $
             [ KTree K_SDoms (map (keyTree . domOfGF ) $ M.elems doms )
             , KTree K_SExprs (map keyTree exprs)
             ] ++ maybeToList (fmap (KTree K_SObj . (: []) . keyTree) obj)


instance GetKey (OObjective, Expr) where
  getKey (o,_)     = fromString . show . toConstr $ o
  keyTree d@(_,e) = KTree (getKey d) [keyTree e]


instance GetKey Type where
  getKey x   = fromString . show . toConstr $ x
  keyTree d  =  KTree (getKey d) (map keyTree $ children d)

instance GetKey Expr where
  getKey x = fromString . show . toConstr $ x

  keyTree d@(EVar _)               = KTree (getKey d) []
  keyTree d@(ECon x)               = KTree (getKey d) [keyTree x]
  keyTree d@(ELit x)               = KTree (getKey d) [keyTree x]
  keyTree d@(EOp x)                = KTree (getKey d) [keyTree x]
  keyTree d@(ETyped _ x2)          = KTree (getKey d) [keyTree x2]
  keyTree d@(EDom x)               = KTree (getKey d) [keyTree x]

  keyTree d@(EComp x1 x2 x3)       = KTree (getKey d) (keyTree x1 :map keyTree x2 ++  map keyTree x3)

  keyTree d@EEmptyGuard            = KTree (getKey d) []
  keyTree d = docError ["unhandled " <+> pretty d <+> "in buildKTree" ]


instance GetKey EGen  where
  getKey _     = K_EGen
  keyTree e@(GenDom p dom) = KTree (getKey e) [keyTree p, keyTree dom]
  keyTree e@(GenIn p dom)  = KTree (getKey e) [keyTree p, keyTree dom]


instance GetKey AbstractPattern  where
  getKey x           = fromString . show . toConstr $ x
  keyTree e@Single{} = KTree (getKey e) []
  keyTree e          = KTree (getKey e) (map keyTree (childrenBi e :: [AbstractPattern]) )


instance (GetKey a, ExpressionLike a) => GetKey (Op a) where
  getKey x  = fromString . drop 2 .  show . toConstr $ x
  keyTree d =  KTree (getKey d) (map keyTree $ (childrenBi d :: [a]) )


instance GetKey Constant where
  getKey x = fromString . show . toConstr $ x

  keyTree d@(ConstantAbstract x) = KTree (getKey d) [keyTree x]
  keyTree d@(ConstantInt x)      = KTree (getKey d) [keyTree x]
  keyTree d                      = KTree (getKey d) []


instance GetKey Integer where
  getKey x | x < 0 || x > 10  = K_Int_Other
  getKey x  = fromString $ "Int_" ++ show (abs x)
  keyTree x = KTree (getKey x) []


instance GetKey a => GetKey (AbstractLiteral a) where
  getKey x = fromString . show . toConstr $ x

  keyTree d@(AbsLitTuple x)                = KTree (getKey d) (map keyTree x)
  keyTree d@(AbsLitRecord x)               = KTree (getKey d) (map (keyTree . snd) x)
  keyTree d@(AbsLitVariant Nothing _ x3)   = KTree (getKey d) [keyTree x3]
  keyTree d@(AbsLitVariant (Just x1) _ x3) = KTree (getKey d) (keyTree x3 :
                                                              map (keyTree . snd) x1)
  keyTree d@(AbsLitMatrix x1 x2)           = KTree (getKey d) (keyTree x1 :
                                                              map keyTree x2)
  keyTree d@(AbsLitSet x)                  = KTree (getKey d) (map keyTree x)
  keyTree d@(AbsLitMSet x)                 = KTree (getKey d) (map keyTree x)
  keyTree d@(AbsLitFunction x)             = KTree (getKey d) (concatMap kt x)
                                             where kt (a,b) = map keyTree [a,b]
  keyTree d@(AbsLitSequence x)             = KTree (getKey d) (map keyTree x)
  keyTree d@(AbsLitRelation x)             = KTree (getKey d) (map keyTree (concat x))
  keyTree d@(AbsLitPartition x)            = KTree (getKey d) (map keyTree (concat x))


instance GetKey a => GetKey (Range a) where
  getKey x = fromString . show . toConstr $ x

  keyTree d@RangeOpen             = KTree (getKey d) ([])
  keyTree d@(RangeSingle x)       = KTree (getKey d) ([keyTree x])
  keyTree d@(RangeLowerBounded x) = KTree (getKey d) ([keyTree x])
  keyTree d@(RangeUpperBounded x) = KTree (getKey d) ([keyTree x])
  keyTree d@(RangeBounded x1 x2)  = KTree (getKey d) ([keyTree x1, keyTree x2])


instance GetKey a => GetKey (Domain () a) where
  getKey x = fromString . show . toConstr $ x

  keyTree d@DomainBool                  = KTree (getKey d) ([])
  keyTree d@DomainIntEmpty              = KTree (getKey d) ([])
  keyTree d@(DomainInt x)               = KTree (getKey d) (map keyTree x)
  keyTree d@(DomainEnum _ _ _)          = KTree (getKey d) ([])
  keyTree d@(DomainUnnamed _ _)         = KTree (getKey d) ([])
  keyTree d@(DomainTuple x)             = KTree (getKey d) (map keyTree x)
  keyTree d@(DomainRecord x)            = KTree (getKey d) (map (keyTree . snd) x)
  keyTree d@(DomainVariant x)           = KTree (getKey d) (map (keyTree . snd) x)
  keyTree d@(DomainMatrix x1 x2)        = KTree (getKey d) (map keyTree [x1,x2])
  keyTree d@(DomainSet _ x2 x3)         = KTree (getKey d) [keyTree x2, keyTree x3]
  keyTree d@(DomainMSet _ x2 x3)        = KTree (getKey d) [keyTree x2, keyTree x3]
  keyTree d@(DomainFunction _ x2 x3 x4) = KTree (getKey d) [keyTree x2, keyTree x3,keyTree x4]
  keyTree d@(DomainSequence _ x2 x3)    = KTree (getKey d) [keyTree x2, keyTree x3]
  keyTree d@(DomainRelation _ x2 x3)    = KTree (getKey d) (keyTree x2 : map keyTree x3)
  keyTree d@(DomainPartition _ x2 x3)   = KTree (getKey d) [keyTree x2, keyTree x3]

  keyTree d = docError ["unhandled " <+> pretty d <+> "in buildKTree" ]


instance GetKey a => GetKey (FunctionAttr a) where
  getKey x  = fromString . show . toConstr $ x
  keyTree d@(FunctionAttr d1 d2 d3) = KTree (getKey d) ([keyTree d1, keyTree d2, keyTree d3])

instance GetKey a => GetKey (MSetAttr a) where
  getKey x  = fromString . show . toConstr $ x
  keyTree d@(MSetAttr d1 d2) = KTree (getKey d) ([keyTree d1, keyTree d2])

instance GetKey a => GetKey (PartitionAttr a) where
  getKey x  = fromString . show . toConstr $ x
  keyTree d = KTree (getKey d) ([])

instance GetKey a => GetKey (RelationAttr a) where
  getKey x  = fromString . show . toConstr $ x
  keyTree d@(RelationAttr d1 d2) = KTree (getKey d) ([keyTree d1, keyTree d2])

instance GetKey a => GetKey (SequenceAttr a) where
  getKey x  = fromString . show . toConstr $ x
  keyTree d@(SequenceAttr d1 d2) = KTree (getKey d) ([keyTree d1, keyTree d2])

instance GetKey a => GetKey (SetAttr a) where
  getKey x  = fromString . show . toConstr $ x
  keyTree d@(SetAttr d1) = KTree (getKey d) ([keyTree d1])


instance GetKey a => GetKey (SizeAttr a) where
  getKey x  = fromString . show . toConstr $ x
  keyTree d = KTree (getKey d) (map keyTree $ (childrenBi d :: [a]) )

instance GetKey a => GetKey (OccurAttr a) where
  getKey x  = fromString . show . toConstr $ x
  keyTree d = KTree (getKey d) (map keyTree $ (childrenBi d :: [a]) )

instance GetKey PartialityAttr where
  getKey x  = fromString . show . toConstr $ x
  keyTree d = KTree (getKey d) []

instance GetKey JectivityAttr where
  getKey x  = fromString . show . toConstr $ x
  keyTree d = KTree (getKey d) []

instance GetKey BinaryRelationAttrs where
  getKey x  = fromString . show . toConstr $ x
  keyTree d@(BinaryRelationAttrs d1) = KTree (getKey d) (map keyTree $ S.toList d1)

instance GetKey BinaryRelationAttr where
  getKey x  = fromString . show . toConstr $ x
  keyTree d = KTree (getKey d) []
