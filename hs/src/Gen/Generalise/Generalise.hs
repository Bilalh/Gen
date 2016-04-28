{-# LANGUAGE Rank2Types #-}
module Gen.Generalise.Generalise where

import Gen.Generalise.Data
import Gen.Generalise.Runner
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.RunResult
import Gen.Reduce.Data       (RConfig (..), addLog)
import Gen.Reduce.Random
import Gen.Reduce.Reduction
import Gen.Helpers.MonadNote
import Gen.Instance.Point
import Gen.Reduce.Transform   (deEnum)
import Gen.Reduce.Point

import Data.Generics.Uniplate.Zipper (Zipper, fromZipper, hole, replaceHole,
                                      zipperBi)

import qualified Data.Generics.Uniplate.Zipper as Zipper


generaliseMain :: (MonadIO m, MonadLog m, RndGen m, MonadNote m)
               =>  GState -> m GState
generaliseMain ee = do
  let base = (specDir_ . rconfig) ee
      fp   =  base </> "spec.spec.json"

  sp_ <- liftIO $ readFromJSON fp

  -- Remove quantification and enums
  let paramFp_ = base </> "given.param"
  paramFp <- liftIO $ doesFileExist paramFp_  >>= \case
    False -> return Nothing
    True  -> return (Just paramFp_)

  (sp,startParam') <-  liftIO $ deEnum sp_ paramFp
  liftIO $ checkForParamIfNeeded sp startParam'
  let startParam = case startParam' of
        Just (Point []) -> Nothing
        x               -> x
  noteFormat "Starting with" [pretty sp, pretty startParam]

  (sfin,state) <- runIdentityT $  (flip runStateT) ee $
      return (sp, startParam)
      >>= (noted "ConstraintsWithSingle") generaliseConstraintsWithSingle
      >>= (noted "NewInstances")          generaliseNewInstances
      >>= \ret -> get >>= \g -> addLog "FinalState" [pretty g] >> return ret


  noteFormat "State" [pretty state]
  noteFormat "Start" [pretty  (sp,startParam)]
  noteFormat "Final" [pretty sfin]

  return (state)

  where
  noted tx f s = do
      noteFormat ("@" <+> tx <+> "Start") []

      newSp <- f s
      noteFormat ("@" <+> tx <+> "End") [pretty newSp]

      return newSp


generaliseNewInstances :: (Spec, Maybe Point)  -> EEE (Spec, Maybe Point)
generaliseNewInstances x@(_,Nothing) = return x
generaliseNewInstances x@(spec,_) = do
  mayPoints <- replicateM 5 $ generatePoint spec
  let points :: [Point] = catMaybes mayPoints
  mapM_ (\p -> runSpec2 spec (Just p)) points
  return x


generaliseConstraintsWithSingle :: (Spec, Maybe Point)  -> EEE (Spec, Maybe Point)
generaliseConstraintsWithSingle (sp,mayP) = do
  let (specZipper :: SpecZipper) = fromJustNote "generaliseCons" $ zipperBi sp
  forM_ (allContextsExcept specZipper) $ \ x -> do
    let ehole =  hole x
    singles   <- runSingle ehole

    forM_ singles $ \s -> do
      let whole = fromZipper (replaceHole s x)
      -- liftIO $ print . pretty $ whole
      runSpec2 whole mayP

    return ()

  return (sp, mayP)


type SpecZipper = Zipper Spec Expr

allContextsExcept :: SpecZipper -> [SpecZipper]
allContextsExcept z0 = concatMap subtreeOf (allSiblings z0)
    where
        -- the input has to be the left most
        allSiblings :: SpecZipper -> [SpecZipper]
        allSiblings z = z : maybe [] allSiblings (Zipper.right z)

        subtreeOf :: SpecZipper -> [SpecZipper]
        subtreeOf z = z : case hole z of
            EVar{}   -> []     -- don't go through a Reference
            ETyped{} -> []     -- don't go through a Typed
            _      -> maybe [] allContextsExcept (Zipper.down z)


recordResult :: ErrData -> EEE ()
recordResult r= do
  modify $ \st -> st{choicesToUse_ =Just (choices r) }
  return ()