{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Expr where

import Conjure.Language.Definition
import Conjure.Language.Expression.Op
import Gen.Essence.Constant           ()
import Gen.Essence.Domain             ()
import Gen.Essence.Literal            ()
import Gen.Essence.Op                 ()
import Gen.Essence.Range              ()
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.TypeOf
import Gen.Imports

import qualified Data.Foldable as F
import qualified Data.Map      as M
import qualified Text.PrettyPrint as Pr

instance Generate Expr where
  give g  = do
    let defs =
          [ (possible (Proxy :: Proxy Constant),               (K_ECon,  ECon  <$> give g))
          , (possible (Proxy :: Proxy  Var),                   (K_EVar,  EVar  <$> give g))
          , (possible (Proxy :: Proxy (Op Expr)),              (K_EOp,   EOp   <$> give g))
          , (possible (Proxy :: Proxy ListComp),               (K_EComp, wrapComp <$> give g))
          , (possible (Proxy :: Proxy (AbstractLiteral Expr)), (K_ELit,  wrapLiteral <$> give g))
          ]

    parts <- getPossibilities g defs
    frequency3 parts

    where
    -- Put a Typed around empty lits e.g a empty set
    wrapLiteral ::  AbstractLiteral Expr -> Expr
    wrapLiteral a = ELit a

    wrapComp (a,b,c) = EComp a b c

  possible _ _ = return True

instance Generate ListComp where
  give GNone = give (GType $ TypeMatrix TypeInt TypeBool)

  give (GType (TypeMatrix TypeInt a)) = do
    gen <- vectorOf3 1 (withDepthDec $ give (GType a))
    is  <- give (GType a)
    cs  <- return []
    return (is, gen, cs)

  give t = giveUnmatched "Generate ListComp" t

  possiblePure _ (TypeMatrix TypeInt _) d  | d > 1 = True
  possiblePure _ _ _ = False
  possibleNoType _ _ = False

instance Generate EGen where
  give GNone       = give (GType TypeInt)
  give con@GType{} = do
    dom <- give con
    return $ GenDom (Single $ Name "x") dom


  give t = giveUnmatched "Generate EGen" t

  possible _ _ = return True


instance Generate Var where
  give (GType ty) = do
    ds <- gets doms_
    let ks = M.toList . M.filter (== ty) . M.map (typeOfDom . domOfGF) $ ds
    let choices =  map (return . uncurry Var) ks
    oneof3 choices

  give t = giveUnmatched "Generate Var" t

  possible _ (GType ty) = do
    ds <- gets doms_
    F.foldrM f False ds

    where
    f _  True  = return True
    f gf False = do
       b <- ttypeOf gf
       return $ b == ty

  possible _ _ = return False

instance Pretty [Expr] where
    pretty = Pr.brackets  . prettyArr

instance Pretty [EGen] where
    pretty = Pr.brackets  . prettyArr
