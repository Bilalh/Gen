{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Expr where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Helpers.SizeOf
import Gen.Helpers.StandardImports
import Gen.Helpers.TypeOf
import Gen.Essence.Rnd
import Gen.Essence.Type()
import Gen.Essence.Op()
import Gen.Essence.Range()
import Gen.Essence.Literal()
import Gen.Essence.Constant()

import qualified Data.Foldable as F
import qualified Data.Map      as M




instance Generate Expr where
  give g  = do
    let defs =
          [ (possible (Proxy :: Proxy Constant), ("ECon",  ECon <$> give g))
          , (possible (Proxy :: Proxy  Var),      ("EVar",  EVar  <$> give g))
          , (possible (Proxy :: Proxy (Op Expr)),  ("EOp",   EOp  <$> give g))
          , (possible (Proxy :: Proxy (AbstractLiteral Expr)), ("ELit",  wrapLiteral <$> give g))
          ]

    parts <- getPossibilities g defs
    frequency3 parts

    where
    -- Put a Typed around empty lits e.g a empty set
    wrapLiteral ::  AbstractLiteral Expr -> Expr
    wrapLiteral a = ELit a

  possiblePure _ _ _ = True


instance Generate Var where
  give (GType ty) = do
    ds <- gets doms_
    let ks = M.toList . M.filter (== ty) . M.map (typeOfDom . domOfGF) $ ds
    let choices =  map (return . uncurry Var) ks
    oneof3 choices

  give t = giveUnmatched "Generate Var" t

  possible _ ty = do
    ds <- gets doms_
    F.foldrM f False ds

    where
    f _  True  = return True
    f gf False = do
       b <- ttypeOf gf
       return $ b == ty
