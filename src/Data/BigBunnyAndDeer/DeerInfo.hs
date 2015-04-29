module Data.BigBunnyAndDeer.DeerInfo
  ( DeerEntry(..)
  , getDeerInfo
  , findDeerEntry
  , updateFreqInfo
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
    ct <- getCurrentTimestamp
    let (DeerEntry c _) = findDeerEntry fi did
        entry = (did, DeerEntry (succ c) (Just ct))
        newFI = (entry :) -- :)
              . filter ((/= did) . fst)
              . IM.toList
              $ fi
    writeFile deerInfoFilePath (unlines (map (show . second deerEntryToPair) newFI))

deerInfoFilePath :: FilePath
deerInfoFilePath = "deerinfo.txt"

getRawDeerInfo :: IO String
getRawDeerInfo = SIO.readFile deerInfoFilePath

parseLine :: String -> (Int, (Int, Maybe Integer))
parseLine = read

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
