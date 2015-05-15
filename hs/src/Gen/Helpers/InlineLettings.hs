module Gen.Helpers.InlineLettings where

import Conjure.Language.Definition
import Gen.Imports
import Conjure.Language.CategoryOf

inlineLettings :: Model -> Model
inlineLettings model =
    let
        inline p@(Reference nm _) = do
            x <- gets (lookup nm)
            return (fromMaybe p x)
        inline p = return p

        statements = catMaybes
                        $ flip evalState []
                        $ forM (mStatements model)
                        $ \ st ->
            case st of
                Declaration (Letting nm x)
                    | categoryOf x /= CatDecision
                    -> modify ((nm,x) :) >> return Nothing
                -- The following doesn't work when the identifier is used in a domain
                -- Declaration (Letting nm x@Reference{})
                --     -> modify ((nm,x) :) >> return Nothing
                _ -> Just <$> transformBiM inline st
    in
        model { mStatements = statements }
