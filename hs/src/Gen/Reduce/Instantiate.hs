module Gen.Reduce.Instantiate where

import Gen.Imports
import Conjure.Language.Definition
import Conjure.Language.Domain

instantiateGenerators :: Expr -> [Expr]
instantiateGenerators (EComp inn gens cs )  =
  let possible = mapMaybe allow gens
  in  concatMap process possible


  where
    allow :: EGen -> Maybe (Text, [Constant])
    allow (GenDom (Single (Name name)) dom) =  allow1 name dom
    allow _ = Nothing

    allow1 name DomainBool{} = Just (name, [ ConstantBool True
                                         , ConstantBool False] )

    allow1 name (DomainInt rs) =
      case rangesInts rs of
        Left{} ->  Nothing
        Right xs ->
          let vals = nub2 $ take 2 xs ++ (reverse . take 2 $ xs)
          in  Just (name, [ ConstantInt v  | v <- vals] )

    allow1 _ _ = Nothing

    process :: (Text, [Constant]) -> [Expr]
    -- Would not pass the `simpler` check
    -- process (name, vals) = [ instantiateExpr [(name,v)] comp  | v <- vals ]
    process (name, vals) =
      [instantiateExpr [(name,v)]
        (EComp inn (filter (withOut name) gens) cs) | v <- vals]

    withOut name (GenDom (Single (Name nm)) _) = name /= nm
    withOut _ _ = True



instantiateGenerators _ = []

instantiateExpr :: [(Text, Constant)] ->  Expr -> Expr
instantiateExpr vals expr =
    let
        inline p@(EVar (Var nm _) ) =
            let x = ECon <$> lookup nm vals
            in fromMaybe p x
        inline p =  p

    in transformBi inline (expr)
