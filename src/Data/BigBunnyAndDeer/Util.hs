module Data.BigBunnyAndDeer.Util
  ( getCurrentTimestamp
  ) where

import Data.Time
import Control.Applicative

getCurrentTimestamp :: IO Integer
getCurrentTimestamp = utcToInteger <$> getCurrentTime
  where
    tZero = UTCTime (ModifiedJulianDay 0) 0
    -- get millseconds for better precision
    utcToInteger :: UTCTime -> Integer
    utcToInteger = truncate . (* 1000) . (`diffUTCTime` tZero)
