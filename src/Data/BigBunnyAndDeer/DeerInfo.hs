module Data.BigBunnyAndDeer.DeerInfo
  ( findDeerEntry
  , updateDeerInfo
  , writeDeerInfo
  , parseRawDeerInfo
  , fetchDeerInfo
  ) where

import Data.Maybe
import Control.Arrow
import Control.Monad.IO.Class
import System.Directory
import qualified System.IO.Strict as SIO
import qualified Data.IntMap as IM
import Data.Default
import Data.Coerce

import Data.BigBunnyAndDeer.Type

deerEntryToPair :: DeerEntry -> (Int, Maybe Integer)
deerEntryToPair (DeerEntry a b) = (a,b)

updateDeerInfo :: DeerId -> Integer -> DeerInfo -> DeerInfo
updateDeerInfo did newTS = coerce $ IM.alter (Just . alterEntry) did
  where
    alterEntry :: Maybe DeerEntry -> DeerEntry
    alterEntry old = case old of
      Nothing -> DeerEntry 1 (Just newTS)
      Just (DeerEntry tt _) -> DeerEntry (succ tt) (Just newTS)


fetchDeerInfo :: FilePath -> IO DeerInfo
fetchDeerInfo fp = do
    b <- liftIO $ doesFileExist fp
    if b
       then parseRawDeerInfo <$> getRawDeerInfo fp
       else return (coerce (def :: IM.IntMap DeerEntry))

getRawDeerInfo :: FilePath -> IO String
getRawDeerInfo = SIO.readFile

parseLine :: String -> (Int, (Int, Maybe Integer))
parseLine = read

dumpDeerInfo :: DeerInfo -> String
dumpDeerInfo =
    coerce >>>
    IM.toList >>>
    map (second deerEntryToPair >>> show) >>>
    unlines

parseRawDeerInfo :: String -> DeerInfo
parseRawDeerInfo =
    lines >>>
    map (parseLine >>> second (uncurry DeerEntry)) >>>
    IM.fromList >>> DeerInfo

findDeerEntry :: DeerInfo -> DeerId -> DeerEntry
findDeerEntry di did = fromMaybe def (IM.lookup did (coerce di))

writeDeerInfo :: FilePath -> DeerInfo -> IO ()
writeDeerInfo fp di = writeFile fp (dumpDeerInfo di)
