{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module EssenceSolver.Args where

import EssenceSolver.Data

import Language.E.Pipeline.ReadIn(readSpecFromFile)

import Build_autoversion(buildDateRFC2822,autoVersion)

import System.Console.CmdArgs.Implicit
import System.FilePath( (<.>), dropExtension )

data Args = Args{  essence     :: FilePath
                 , param       :: Maybe FilePath
                 , outPath     :: Maybe FilePath
                }  deriving (Show, Data, Typeable)


argsDef :: Args
argsDef  = Args
             { essence =  "" &= argPos 0
             , param   = def &= help "Essence Param"
             , outPath = def &= help "Where to output the solution"
             }
         &= summary (unlines ["EssenceSolver Version 1.0"
                             , "Git version: " ++ autoVersion
                             , "Built date: "  ++ buildDateRFC2822
                             ])
         &= helpArg [name "h"]

parseArgs :: IO SolverArgs
parseArgs = do
   Args{..} <- cmdArgs argsDef

   sEssence <- readSpecFromFile essence

   let sOutPath =
           case outPath of
               Just s  ->  s
               Nothing -> dropExtension essence <.> "solution"

   sParam <-
        case param of
            Nothing -> return Nothing
            Just s  -> do
                sp <- readSpecFromFile s
                return $ Just sp

   return $ SolverArgs{..}

    where
    _f Nothing n = error $ "--" ++ n ++ " needs to be specifed"
    _f (Just v) _   = v

