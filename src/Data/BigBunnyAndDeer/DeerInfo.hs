module Data.BigBunnyAndDeer.DeerInfo
  ( DeerEntry(..)
  , DeerInfo
  , getDeerInfo
  , findDeerEntry
  , updateFreqInfo
  , updateDeerInfo
  , writeDeerInfo
  ) where

import Data.Maybe
import Control.Applicative
import Control.Arrow
import System.Directory
import qualified System.IO.Strict as SIO
import qualified Data.IntMap as IM
import Data.Default

import Data.BigBunnyAndDeer.Type
import Data.BigBunnyAndDeer.Util

data DeerEntry = DeerEntry
  { totalTime  :: Int
  , lastAccess :: Maybe Integer
  }

instance Default DeerEntry where
    def = DeerEntry def def

type DeerInfo = IM.IntMap DeerEntry

deerEntryToPair :: DeerEntry -> (Int, Maybe Integer)
deerEntryToPair (DeerEntry a b) = (a,b)

updateFreqInfo :: DeerId -> IO ()
updateFreqInfo did = do
    fi <- getDeerInfo
    ct <- Just <$> getCurrentTimestamp
    let alterEntry :: Maybe DeerEntry -> DeerEntry
        alterEntry old = case old of
          Nothing -> DeerEntry 1 ct
          Just (DeerEntry tt _) -> DeerEntry (succ tt) ct
        newFI = IM.alter (Just . alterEntry) did fi
    writeDeerInfo newFI

updateDeerInfo :: DeerId -> Integer -> DeerInfo -> DeerInfo
updateDeerInfo did newTS = IM.alter (Just . alterEntry) did
  where
    alterEntry :: Maybe DeerEntry -> DeerEntry
    alterEntry old = case old of
      Nothing -> DeerEntry 1 (Just newTS)
      Just (DeerEntry tt _) -> DeerEntry (succ tt) (Just newTS)


writeDeerInfo :: DeerInfo -> IO ()
writeDeerInfo di = writeFile deerInfoFilePath (dumpDeerInfo di)

deerInfoFilePath :: FilePath
deerInfoFilePath = "deerinfo.txt"

getRawDeerInfo :: IO String
getRawDeerInfo = SIO.readFile deerInfoFilePath

parseLine :: String -> (Int, (Int, Maybe Integer))
parseLine = read

dumpDeerInfo :: DeerInfo -> String
dumpDeerInfo =
    IM.toList >>>
    map (second deerEntryToPair >>> show) >>>
    unlines

parseRawDeerInfo :: String -> DeerInfo
parseRawDeerInfo =
    lines >>>
    map (parseLine >>> second (uncurry DeerEntry)) >>>
    IM.fromList

getDeerInfo :: IO DeerInfo
getDeerInfo = do
    b <- doesFileExist deerInfoFilePath
    if b
       then parseRawDeerInfo <$> getRawDeerInfo
       else return def

findDeerEntry :: DeerInfo -> DeerId -> DeerEntry
findDeerEntry di did = fromMaybe def (IM.lookup did di)
