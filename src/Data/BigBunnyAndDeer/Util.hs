module Data.BigBunnyAndDeer.Util
  ( getCurrentTimestamp
  , choice
  ) where

import Data.Time
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Random

getCurrentTimestamp :: (MonadIO m, Functor m) => m Integer
getCurrentTimestamp = utcToInteger <$> liftIO getCurrentTime
  where
    tZero = UTCTime (ModifiedJulianDay 0) 0
    -- get millseconds for better precision
    utcToInteger :: UTCTime -> Integer
    utcToInteger = truncate . (* 1000) . (`diffUTCTime` tZero)

choice :: (Functor m, MonadRandom m) => [a] -> m a
choice xs = (xs !!) <$> getRandomR (0, l-1)
  where
    l = length xs
