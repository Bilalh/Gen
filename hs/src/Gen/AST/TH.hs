{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Gen.AST.TH(domain, domainn,essencee,opp,conn,litt) where

import Conjure.Language.Definition

import Gen.AST.Imports

import Conjure.Prelude
import Conjure.Language.Domain
import Conjure.Language.Parser

import Text.Megaparsec.Prim ( setPosition )
import Text.Megaparsec.Pos ( SourcePos, newPos )

import Language.Haskell.TH ( Q, runIO, Loc(..), location, mkName, ExpQ, varE, appE, PatQ, varP )
import Language.Haskell.TH.Quote ( QuasiQuoter(..), dataToExpQ, dataToPatQ )

import Data.Generics.Aliases ( extQ )

-- For matching in Reduce and for ghci
essencee, opp, conn, litt :: QuasiQuoter
essencee = ee (id :: Expr -> Expr)
opp      = ee (\(EOp x)  -> x)
conn     = ee (\(ECon x) -> x)
litt     = ee (\(ELit x) -> x)

ee :: forall ast a.
      (Translate ast Expression, Data a) =>
      (ast -> a) -> QuasiQuoter
ee after  = QuasiQuoter
    { quoteExp = \ str -> do
        l <- locationTH
        e <- runIO $ parseIO (setPosition l *> parseExpr) str
        let f = fromConjureNote "in cc quoteExp TH" e
        dataToExpQ (const Nothing `extQ` expEg `extQ` expDg `extQ` expAPg) (after f)
    , quotePat  = \ str -> do
        l <- locationTH
        e <- runIO $ parseIO (setPosition l *> parseExpr) str
        let f = fromConjureNote "in cc quotePat TH" e
        dataToPatQ (const Nothing `extQ` patEg `extQ` patDg `extQ` patAPg) (after f)
    , quoteType = error "quoteType"
    , quoteDec  = error "quoteDec"
    }



domain :: QuasiQuoter
domain = QuasiQuoter
    { quoteExp = \ str -> do
        l <- locationTH
        e <- runIO $ parseIO (setPosition l *> parseDomain) str
        dataToExpQ (const Nothing `extQ` expE `extQ` expD `extQ` expAP) e
    , quotePat  = \ str -> do
        l <- locationTH
        e <- runIO $ parseIO (setPosition l *> parseDomain) str
        dataToPatQ (const Nothing `extQ` patE `extQ` patD `extQ` patAP) e
    , quoteType = error "quoteType"
    , quoteDec  = error "quoteDec"
    }

domainn :: QuasiQuoter
domainn = QuasiQuoter
    { quoteExp = \ str -> do
        l <- locationTH
        e <- runIO $ parseIO (setPosition l *> parseDomain) str
        let f :: Domain () Expr = fromConjureNote "in domainn quoteExp TH" e
        dataToExpQ (const Nothing `extQ` expEg `extQ` expDg  `extQ` expAPg ) f
    , quotePat  = \ str -> do
        l <- locationTH
        e <- runIO $ parseIO (setPosition l *> parseDomain) str
        let f :: Domain () Expr = fromConjureNote "in domainn quotePat TH" e
        dataToPatQ (const Nothing `extQ` patEg `extQ` patDg `extQ` patAPg) f
    , quoteType = error "quoteType"
    , quoteDec  = error "quoteDec"
    }


locationTH :: Q SourcePos
locationTH = aux <$> location
    where
        aux :: Loc -> SourcePos
        aux loc = uncurry (newPos (loc_filename loc)) (loc_start loc)

expE :: Expression -> Maybe ExpQ
expE (ExpressionMetaVar x) = Just [| $(varE (mkName x)) |]
expE _ = Nothing

expD :: Domain () Expression -> Maybe ExpQ
expD (DomainMetaVar x) = Just $ [| $(varE (mkName "forgetRepr")) |]
                         `appE` [| "TH:expD" |]
                         `appE` [| $(varE (mkName x)) |]
expD _ = Nothing

expAP :: AbstractPattern -> Maybe ExpQ
expAP (AbstractPatternMetaVar x) = Just [| $(varE (mkName x)) |]
expAP _ = Nothing


patE :: Expression -> Maybe PatQ
patE (ExpressionMetaVar x) = Just (varP (mkName x))
patE _ = Nothing

patD :: Domain () Expression -> Maybe PatQ
patD (DomainMetaVar x) = Just (varP (mkName x))
patD _ = Nothing

patAP :: AbstractPattern -> Maybe PatQ
patAP (AbstractPatternMetaVar x) = Just (varP (mkName x))
patAP _ = Nothing


expEg :: Expr -> Maybe ExpQ
expEg (EMetaVar x) = Just [| $(varE (mkName x)) |]
expEg _ = Nothing

expDg :: Domain () Expr -> Maybe ExpQ
expDg (DomainMetaVar x) = Just $ [| $(varE (mkName "forgetRepr")) |]
                         `appE` [| "TH:expD" |]
                         `appE` [| $(varE (mkName x)) |]
expDg _ = Nothing

expAPg :: AbstractPattern -> Maybe ExpQ
expAPg (AbstractPatternMetaVar x) = Just [| $(varE (mkName x)) |]
expAPg _ = Nothing


patEg :: Expr -> Maybe PatQ
patEg (EMetaVar x) = Just (varP (mkName x))
patEg _ = Nothing

patDg :: Domain () Expr -> Maybe PatQ
patDg (DomainMetaVar x) = Just (varP (mkName x))
patDg _ = Nothing

patAPg :: AbstractPattern -> Maybe PatQ
patAPg (AbstractPatternMetaVar x) = Just (varP (mkName x))
patAPg _ = Nothing
