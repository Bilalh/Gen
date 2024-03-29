module Gen.Helpers.InlineLettings where

import Conjure.Language.Definition
import Gen.Imports

inlineLettings :: Model -> Model
inlineLettings model =
    let
        inline p@(Reference nm _) = do
            x <- gets (lookup nm)
            return (fromMaybe p x)
        inline p = return p

        statements :: [Statement]
        statements = catMaybes
                        $ flip evalState []
                        $ forM (mStatements model)
                        $ \(st :: Statement) ->
            case st of
                Declaration (Letting nm x)
                    -> modify ((nm,x) :) >> return Nothing
                -- The following doesn't work when the identifier is used in a domain
                -- Declaration (Letting nm x@Reference{})
                --     -> modify ((nm,x) :) >> return Nothing
                st' -> Just <$> transformBiM inline (st' :: Statement)
    in
        model { mStatements = statements }
