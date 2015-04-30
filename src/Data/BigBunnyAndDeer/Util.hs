module Data.BigBunnyAndDeer.Util
  ( getCurrentTimestamp
  ) where

import Data.Time
import Control.Applicative
import Control.Monad.IO.Class

getCurrentTimestamp :: (MonadIO m, Functor m) => m Integer
getCurrentTimestamp = utcToInteger <$> liftIO getCurrentTime
  where
    tZero = UTCTime (ModifiedJulianDay 0) 0
    -- get millseconds for better precision
    utcToInteger :: UTCTime -> Integer
    utcToInteger = truncate . (* 1000) . (`diffUTCTime` tZero)
