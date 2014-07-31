{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module EssenceSolver.EssenceSolver where

import EssenceSolver.Data
import EssenceSolver.Solve

import Bug
import Language.E
import Language.E.ValidateSolution

import Language.E.Pipeline.ExplodeStructuralVars ( explodeStructuralVars )
import Language.E.Pipeline.InlineLettings ( inlineLettings )
import Language.E.Pipeline.ReadIn(readSpecFromFile)

import System.FilePath( (<.>), (</>))

main' :: SolverArgs -> IO ()
main' SolverArgs{..} = do
    print . pretty $ sEssence
    let (spec@(Spec _ _),specLogTree) = inlineParamAndLettings sEssence sParam

    -- print . pretty $ specLogTree
    print . pretty $ spec

    let solution = solveSpec spec
    putStrLn "\n~~~First Solution~~~"
    print . fmap (map pretty)  $ solution
    return ()


inlineParamAndLettings :: Spec -> Maybe Spec -> (Spec, LogTree)
inlineParamAndLettings essence param =
    let
        (mresult, _logs) = runCompESingle "simplify solution" helper
        in
        case mresult of
            Left  x      -> userErr x
            Right b -> (b, _logs)

    where
    helper = do

        case param of
            Nothing -> return ()
            Just (Spec _ s) -> mapM_ introduceStuff (statementAsList s)

        bindersDoc >>= mkLog "binders 2"

        let essenceCombined =
                case (essence, param) of
                    (Spec l s, Just (Spec _ p)) ->
                        Spec l (listAsStatement $ statementAsList p ++ statementAsList s)
                    _ -> essence

        let pipeline0 = recordSpec "init"
                >=> explodeStructuralVars   >=> recordSpec "explodeStructuralVars"
                >=> inlineLettings          >=> recordSpec "inlineLettings"
                >=> stripDecls              >=> recordSpec "stripDecls"
                -- >=> return . atMostOneSuchThat True >=> recordSpec "atMostOneSuchThat"

        inlined <- pipeline0 essenceCombined
        return $ inlined

stripDecls :: MonadConjure m => Spec -> m Spec
stripDecls (Spec language stmt) = return $ Spec language $ listAsStatement
    [ i
    | i <- statementAsList stmt
    , case i of
        [xMatch| _ := topLevel.declaration.given |] -> False
        [xMatch| _ := topLevel.letting.domain |] -> False
        _ -> True
    ]

_m :: FilePath -> IO ()
_m s = do
    sp <- readSpecFromFile $
        "/Users/bilalh/Desktop/Results/testgen/zspecs/Solve/"
        </> s </> s <.> "essence"
    -- pa <- readSpecFromFile "/Users/bilalh/Desktop/Results/testgen/zspecs/Solve/1/p.param"
    main' $
        SolverArgs{
         sEssence=sp
        -- ,sParam= Just pa
        ,sParam = Nothing
        ,sOutPath="/Users/bilalh/Desktop/Results/testgen/zspecs/Solve/1/1.solution"
        }
