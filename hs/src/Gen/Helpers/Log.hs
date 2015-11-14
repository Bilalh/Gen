module Gen.Helpers.Log
    ( addLog
    , rrError
    ) where

import Gen.Imports
import qualified Text.PrettyPrint as P


addLog :: MonadLog m => String -> [Doc] ->  m ()
addLog t ds = logDebugVerbose $ hang (pretty t) 4 (vcat ds)

rrError :: String -> [Doc] -> m a
rrError title docs = do
  error . show $ ( P.text $ padRight 15 ' ' title  )
      P.$+$ (nest 4 $ vcat (docs ))
      P.$+$ ""
