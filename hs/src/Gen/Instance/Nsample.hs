{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.Instance.Nsample(Nsample(..),Sampling(..)) where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.Method
import Gen.Instance.Point         (pointHash,Distance(..))
import Gen.Instance.RaceRunner    (getPointQuailty)
import Gen.Instance.SamplingError (SamplingErr (ErrDontCountIteration))
import Text.Printf                (printf)

import qualified Data.Aeson as A

data Nsample = Nsample{
      mInfluence_radius :: Int
    }
  deriving (Eq, Show, Data, Typeable, Generic)

instance A.FromJSON Nsample
instance A.ToJSON Nsample

-- Goodness is 1 = Quailty
type Goodness = Double


instance Sampling Nsample where
  doIteration = do
    randomPoint >>= \case
      Left e -> return $ Left e
      Right x -> do
        (Method MCommon{mPoints} _) <- get

        goodness_x <- goodness x
        logWarn2 $line [nn "made point" x]

        goodness_x_prev :: Goodness <- case mPoints of
          (las:_) -> do
            logWarn2 $line ["Using previous data point"]
            (1 -) <$> get_quailty las
          [] -> do
            logWarn2 $line ["No previous data point"]
            return 1

        logInfo2 $line [n3 "goodness_x" goodness_x
                       ,n3 "goodness_x_prev" goodness_x_prev
                       ,nn "point" x]


        let accept_point = do
              if goodness_x_prev == 0 then do
                  logWarn2 $line ["Unconditionally accepting since goodness_x_prev is 0"]
                  return True
              else do
                  let accept = goodness_x / goodness_x_prev
                  if accept >= 1 then do
                      logWarn2 $line [n3 "Unconditionally accepting because of" accept ]
                      return True
                  else do
                      u <- liftIO $ randomRIO (0, 1)
                      logWarn2 $line [ n3 "accept" accept
                                     , n3 "u"      u
                                     , nn "u<=accept" (u <= accept)
                                     ]
                      return $ u <= (accept + 1e-5)

        accept_point >>= \case
          True ->  runPoint x
          False -> return . Left . ErrDontCountIteration $ vcat
                     [n3 "goodness_x" goodness_x
                     ,n3 "goodness_x_prev" goodness_x_prev
                     ,nn "point" x
                     ]


goodness :: (MonadIO m, MonadState (Method Nsample) m, MonadLog m, Sampling Nsample)
         => Point -> m Goodness
goodness point = do
  quality <- avg_quailty point
  return $ 1 - quality

  where
  avg_quailty rp = do
   (Method MCommon{mPoints} Nsample{mInfluence_radius}) <- get
   influence_points <- filterM (shape_is_in_inside mInfluence_radius rp) mPoints
   case influence_points of
     [] -> return 0.5
     _ -> do
       logWarn2 $line [nn "len(influence_points)" (length influence_points)
                      ,nn "len(data_points)" (length mPoints)]
       quailties :: [Quality] <- mapM get_quailty influence_points
       let mean = sum quailties / fromIntegral (length quailties)
       return mean


shape_is_in_inside :: MonadLog m => Int -> Point -> Point -> m Bool
shape_is_in_inside = eucludean_is_in_inside

eucludean_is_in_inside :: MonadLog m => Int -> Point -> Point -> m Bool
eucludean_is_in_inside radius center point = do
  let rooted = distance center point
  let res    = rooted  <= fromIntegral radius
  logDebug2 $line [ nn "distance" rooted
                 , nn "inside?" res
                 , nn "radius" radius
                 , nn "center" center
                 , nn "point"  point
                 ]
  return res


get_quailty :: (MonadIO m, MonadState (Method Nsample) m, MonadLog m, Sampling Nsample)
            => Point -> m Quality
get_quailty p = do
  let h  = pointHash p
  q <- getPointQuailty h
  logDebug2 $line [n3 "qu" q, nn "for hash" h ]
  return q


runPoint :: (MonadIO m, MonadState (Method a) m, MonadLog m, Sampling a)
         => Point -> m (Either SamplingErr ())
runPoint picked = do
  runParamAndStoreQuality picked >>= \case
    Left err -> return $ Left err
    Right{}  -> do
      storeDataPoint picked
      return $ Right ()

n3 :: Doc -> Double -> Doc
n3 a d = a <+> ":"  <+> pretty (printf "%.3f" d :: String) <+> ""
