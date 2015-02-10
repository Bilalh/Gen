module A where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.TH
import Conjure.UI.IO(readModelFromFile)
import Conjure.Language.Pretty

import Conjure.UI.Model

aa :: IO Model
aa = do
  a <- readModelFromFile "/Users/bilalh/Desktop/Results/__int/set_of_first.essence"
  b <- ignoreLogs $ prologue a
  return b
