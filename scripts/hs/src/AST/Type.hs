{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AST.Type where

import AST.Data
import AST.FromEssence(FromEssence(..))
import AST.ToEssence(ToEssence(..))
import Language.E
import Text.Groom(groom)

instance ToEssence Type where
    toEssence TInt          = [dMake| int |]
    toEssence TBool         = [dMake| bool |]

    toEssence (TMatix t)    = [dMake| matrix indexed by [int(1..1)] of &t' |]
        where t' = toEssence t

    toEssence (TSet t)      = [dMake| set of &t' |] where t' = toEssence t
    toEssence (TMSet t)     = [dMake| mset of &t' |] where t' = toEssence t

    toEssence (TFunc t1 t2) = [dMake| function &t1' --> &t2' |]
        where t1' = toEssence t1
              t2' = toEssence t2

    toEssence (TTuple t)    = [xMake| domain.tuple.inners := map toEssence t |]
    toEssence (TRel t)      = [xMake| domain.relation.attributes.attrCollection := []
                                    | domain.relation.inners := map toEssence t |]
    toEssence (TPar t)      = [dMake| partition from &t' |] where t' = toEssence t

    -- toEssence (TUnamed t)   = [dMake| t |]
    -- toEssence (TEnum t)     = [dMake| _a |] where t' = toEssence t

    -- toEssence TAny          =

    toEssence x = error . show . vcat $ [ "ToEssence Type", "x=" <+> (pretty .groom $ x)]


instance FromEssence Type where
    fromEssence [xMatch| [d]  := domainInExpr |] = fromEssence d

    fromEssence [dMatch| int  |]     = return TInt
    fromEssence [dMatch| bool |]     = return TBool

    fromEssence [dMatch| set of &t|]  = TSet <$> fromEssence t
    fromEssence [dMatch| mset of &t|] = TMSet <$> fromEssence t

    fromEssence [dMatch| function &t1 --> &t2 |] = TFunc <$> fromEssence t1
                                                         <*> fromEssence t2

    fromEssence [dMatch| partition from &t |] = TPar <$> fromEssence t

    fromEssence [xMatch| is   := domain.relation.inners
                       | [_] := domain.relation.attributes |] = do
        iv <- mapM fromEssence is
        return $ TRel iv

    fromEssence x = error . show . vcat $ [ "fromEssence Type", "x=" <+> pretty x]

-- Having a Pretty instances is error prone,
-- Since we might want  show or toEssence
-- and toEssecne will break on TAny (as it should)

-- instance Pretty Type where
--     pretty  =  pretty  . toEssence

-- instance Pretty [Type] where
--     pretty  =  vcat . map pretty
