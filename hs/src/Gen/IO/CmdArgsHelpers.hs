module Gen.IO.CmdArgsHelpers where

import Gen.Prelude
import Gen.IO.TermSize

helpArg :: MonadIO m => m String
helpArg = do
  len <- liftIO getHelpLength
  return $ "--help=" ++ show len

replaceOldHelpArg :: MonadIO m => [String] -> m [String]
replaceOldHelpArg ins = mapM f ins
  where
    f "-h"     = helpArg
    f "-?"     = helpArg
    f "--help" = helpArg
    f other    = return other

getHelpLength :: MonadIO m => m Int
getHelpLength = do
    (_,cols) <- liftIO getTermSize
    let helpLength =  getHelpLength' cols
    return helpLength
  where
    getHelpLength' :: Int -> Int
    getHelpLength' len
        | len > 120 = 120
        | len < 60  = 60
        | otherwise = len
