-- really only for usage in ghci
module AST.TH(domain, domainn,essencee) where

import Conjure.Language.Definition

import AST.Imports

import Conjure.Prelude
import Conjure.Language.Domain
import Conjure.Language.Parser

import Text.Parsec ( SourcePos, setPosition )
import Text.Parsec.Pos ( newPos )

import Language.Haskell.TH ( Q, runIO, Loc(..), location, mkName, ExpQ, varE, appE, PatQ, varP )
import Language.Haskell.TH.Quote ( QuasiQuoter(..), dataToExpQ, dataToPatQ )

import Data.Generics.Aliases ( extQ )

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
        let f :: Domainn Expr = fromConjureNote "in domainn quoteExp TH" e
        dataToExpQ (const Nothing `extQ` expE `extQ` expD `extQ` expAP) f
    , quotePat  = \ str -> do
        l <- locationTH
        e <- runIO $ parseIO (setPosition l *> parseDomain) str
        let f :: Domainn Expr = fromConjureNote "in domainn quotePat TH" e
        dataToPatQ (const Nothing `extQ` patE `extQ` patD `extQ` patAP) f
    , quoteType = error "quoteType"
    , quoteDec  = error "quoteDec"
    }

essencee :: QuasiQuoter
essencee = QuasiQuoter
    { quoteExp = \ str -> do
        l <- locationTH
        e <- runIO $ parseIO (setPosition l *> parseExpr) str
        let f :: Expr = fromConjureNote "in essencee quoteExp TH" e
        dataToExpQ (const Nothing `extQ` expE `extQ` expD `extQ` expAP) f
    , quotePat  = \ str -> do
        l <- locationTH
        e <- runIO $ parseIO (setPosition l *> parseExpr) str
        let f :: Expr = fromConjureNote "in essencee quotePat TH" e
        dataToPatQ (const Nothing `extQ` patE `extQ` patD `extQ` patAP) f
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
