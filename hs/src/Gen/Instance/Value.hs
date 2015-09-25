{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.Instance.Value where

import Conjure.Language.Constant
import Conjure.Language.Definition
import Gen.Imports

newtype Point  = Point [(Name,Constant)]
 deriving (Eq, Show, Data, Typeable, Generic)

pointHash :: Point -> String
pointHash _ = "empty"
