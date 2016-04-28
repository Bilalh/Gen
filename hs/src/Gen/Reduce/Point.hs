{-# LANGUAGE Rank2Types #-}
module Gen.Reduce.Point where

import Conjure.Language.Definition
import Conjure.Language.Expression.DomainSizeOf (domainSizeOf)
import Conjure.Process.Enumerate                (EnumerateDomain)
import Conjure.UI.ValidateSolution              (validateSolution)
import Gen.Imports
import Gen.Instance.Point
import Gen.Instance.UI
import Gen.IO.Formats
import Gen.IO.Toolchain                         (writeModelDef)
import Gen.Reduce.Data
import Gen.Reduce.Random
import Gen.Helpers.MonadNote
import Conjure.UserError

import qualified Control.Exception as Exc

-- FIXME this could be done better

-- | Generates a new point for a spec if possible
generatePoint :: (MonadIO m, RndGen m, MonadR m)
              => Spec
              -> m (Maybe Point)
generatePoint spec = do
  RConfig{outputDir_} <- getRconfig
  ts  <- timestamp >>= return . show
  num :: Int <- chooseR (10 :: Int ,99)
  let uname  =  (ts ++ "_" ++ (show num) )
  let tmp = outputDir_ </> "_paramGen" </> uname

  case toConjure spec of
    Nothing      -> return Nothing
    (Just model) -> do
      essencePath <- writeModelDef tmp model
      seed :: Int <- chooseR (0 :: Int , 2^(24 :: Int))
      b <- liftIO $  Exc.catch (
          instances_no_racing essencePath 1 10 tmp Nothing seed LogNone >> return True)
            (\ (_ :: Exc.SomeException) -> return False )
      if not b then
        return Nothing
      else do
        -- FIXME could just the list the dir
        liftIO $ getAllFilesWithSuffix ".param" (tmp </> "_params") >>= \case
           [x] -> do
             p <- readPoint x
             return $ Just p
           _   -> do
             return Nothing

-- | Validate the point with respect with a specification where the givens
-- | are changed to finds
validatePoint1 :: (MonadNote m,  EnumerateDomain m)
               => Spec -> Point
               -> m Bool
validatePoint1 spec ps = do
  let orgModel = toConjureNote "validatePoint1" spec
  converted <- core orgModel
  noteFormat1 "GivensAsFindSpec" (pretty converted)
  validatePoint converted ps

  where
    core :: Monad m => Model -> m Model
    core m = do
      (outStatements, errs) <- runWriterT $ forM (mStatements m) $ \ st -> case st of
        Declaration (FindOrGiven Given nm dom) -> do
          case domainSizeOf dom of
            Nothing -> tell [(nm, dom)] >> return []
            Just (_ :: Expression) -> do
              return [Declaration (FindOrGiven Find nm dom)]

        Declaration (FindOrGiven Find _  _  ) -> return []
        Declaration     {}                    -> return [st]
        SearchOrder     {}                    -> return []
        Where           xs                    -> return [SuchThat xs]
        Objective       {}                    -> return []
        SuchThat        {}                    -> return []
        SearchHeuristic {}                    -> return []

      if null errs
        then return m { mStatements = concat outStatements }
        else docError $      "Given must have a finite domain"
                             : [ pretty nm <> ":" <+> pretty dom
                             | (nm, dom) <- errs
                             ]


-- | Validate the point with respect to an essence specification
validatePoint :: (EnumerateDomain m)
              => Model -> Point
              -> m Bool
validatePoint model (Point parts) = do
  let (givensP, findsP) = partition ( \(n,_) -> n `elem` givesNames ) parts
  let givens =  pointToModel (Point givensP)
  let finds  =  pointToModel (Point findsP)

  x <- runExceptT $ runUserErrorT $  ignoreLogs $ runNameGen $
         validateSolution model givens finds
  case x of
    (Right Right{})  -> return True
    _                -> return False


  where
    givesNames :: [Name]
    givesNames = mapMaybe f (mStatements model)
      where
        f (Declaration (FindOrGiven Given n _)) = Just n
        f _                                     = Nothing
