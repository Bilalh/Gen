{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module AST.OpName where

import Stuff.Pretty
import Data.String ( IsString(..) )
import GHC.Generics ( Generic )
import Data.Serialize ( Serialize(..) )
import Data.Hashable ( Hashable(..) )
import Data.Aeson ( ToJSON(..) )
import qualified Data.Text as T


newtype OpName = OpName T.Text
    deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

instance Serialize OpName where
    put (OpName t) = put (T.unpack t)
    get = fmap (OpName . T.pack) get

instance Pretty OpName where
    pretty (OpName t) = pretty t

instance IsString OpName where
    fromString t | t `elem`
        ["+", "-", "/", "/\\"]
        = OpName (T.pack t)
    fromString t = error $ "Unknown opName: " ++ t
