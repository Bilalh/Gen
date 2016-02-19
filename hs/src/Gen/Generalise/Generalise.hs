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

import Data.Generics.Uniplate.Zipper (Zipper, fromZipper, hole, replaceHole,
                                      zipperBi)

import qualified Data.Generics.Uniplate.Zipper as Zipper


generaliseMain :: (MonadIO m, MonadLog m, RndGen m, MonadNote m)
               =>  GState -> m GState
generaliseMain ee = do
  let base = (specDir_ . rconfig) ee
      fp   =  base </> "spec.spec.json"

  sp :: Spec <- liftIO $ readFromJSON fp
  noteFormat "Starting with" [pretty sp]

  (sfin,state) <- (flip runStateT) ee $
      return sp
      >>= (noted "ConstraintsWithSingle") generaliseConstraintsWithSingle
      >>= \ret -> get >>= \g -> addLog "FinalState" [pretty g] >> return ret


  noteFormat "State" [pretty state]
  noteFormat "Start" [pretty sp]
  noteFormat "Final" [pretty sfin]

  return (state)

  where
  noted tx f s = do
      noteFormat ("@" <+> tx <+> "Start") []

      newSp <- f s
      noteFormat ("@" <+> tx <+> "End") [pretty newSp]

      return newSp


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


generaliseConstraintsWithSingle :: Spec -> EEE Spec
generaliseConstraintsWithSingle sp = do
  let (specZipper :: SpecZipper) = fromJustNote "generaliseCons" $ zipperBi sp
  forM_ (allContextsExcept specZipper) $ \ x -> do
    let pre = fromZipper x
    let ehole =  hole x
    singles   <- runSingle pre ehole

    forM_ singles $ \s -> do
      let whole = fromZipper (replaceHole s x)
      -- liftIO $ print . pretty $ whole
      runSpec2 whole

    return ()

  return sp


recordResult :: ErrData -> EEE ()
recordResult r= do
  modify $ \st -> st{choicesToUse_ =Just (choices r) }
  return ()
