module Gen.Instance.RaceRunner where

import Gen.Imports
import Gen.Instance.Data
import Gen.IO.Formats
import Conjure.UI.IO
import Conjure.Language
import Conjure.Language.NameResolution ( resolveNames )
import Conjure.Language.Expression.DomainSizeOf ( domainSizeOf )

import qualified Data.Set as S

createParamEssence ::  (Sampling a, MonadState (Method a) m, MonadIO m) => m ()
createParamEssence = $notDone



sampleParamFromMinion :: (Sampling a, MonadState (Method a) m, MonadIO m) => Point -> m ()
sampleParamFromMinion = $notDone


createParamSpecification :: (MonadUserError m, MonadFail m) => Model -> VarInfo -> m Model
createParamSpecification model VarInfo{..} = do
  let f = runNameGen . resolveNames  >=> core
  ignoreLogs $ f model

  where
  core m = do
    (outStatements, errs) <- runWriterT $ forM (mStatements m) $ \ st -> case st of
      Declaration (FindOrGiven Given nm@(Name te) dom) ->
        case domainSizeOf dom of
          Nothing -> tell [(nm, dom)] >> return []
          Just (_ :: Expression) -> do
            let k = if te `S.member` givens then Given else Find
            return [Declaration (FindOrGiven k nm dom)]

      Declaration (FindOrGiven Find _  _  ) -> return []
      Declaration {}                        -> return [st]
      SearchOrder {}                        -> return []
      Where       xs                        -> return [SuchThat xs]
      Objective   {}                        -> return []
      SuchThat    {}                        -> return []

    if null errs
      then return m { mStatements = concat outStatements }
      else userErr1 $ vcat $ "Given must have a finite domain"
                           : [ pretty nm <> ":" <++> pretty dom
                           | (nm, dom) <- errs
                           ]


example = do
  i :: VarInfo <- readFromJSON "/Users/bilalh/CS/instancegen-models/_new/prob013-PPP/info.json"
  m <- readModelFromFile "/Users/bilalh/CS/instancegen-models/_new/prob013-PPP/prob013-PPP.essence"
  param <- createParamSpecification m i
  print . pretty $ param
