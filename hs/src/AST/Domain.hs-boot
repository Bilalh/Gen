{-# OPTIONS_GHC -fno-warn-orphans #-}
module AST.Domain where

import AST.ToEssence(ToEssence(..))
import AST.FromEssence(FromEssence(..))
import AST.Data

instance ToEssence Domain
instance FromEssence Domain
instance Pretty Domain

dint, dset, dmset, dmat, dfunc, drel, dpar, dtuple  :: Domain
dintRange :: Integer -> Integer -> Domain