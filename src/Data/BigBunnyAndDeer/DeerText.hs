module Data.BigBunnyAndDeer.DeerText
  ( DeerTextDb
  , allDeerIds
  , lookupDeerText
  , findDeerText
  , fetchDatabase
  ) where

import Control.Applicative
import Data.Maybe
import Control.Arrow
import Network.Curl

import Data.BigBunnyAndDeer.Type
import qualified Data.IntMap as IM

type DeerTextDb = IM.IntMap DeerText

allDeerIds :: DeerTextDb -> [DeerId]
allDeerIds = IM.keys

lookupDeerText :: DeerTextDb -> DeerId -> Maybe DeerText
lookupDeerText = flip IM.lookup

findDeerText :: DeerTextDb -> DeerId -> DeerText
findDeerText db did = fromJust (lookupDeerText db did)

databaseLink :: URLString
databaseLink = "https://gist.githubusercontent.com/Javran/\
             \8595d3587a8aa0c5255f/raw/gistfile1.md"

fetchDatabase :: IO DeerTextDb
fetchDatabase = parseRawDatabase <$> fetchRawDatabase

fetchRawDatabase :: IO String
fetchRawDatabase = do
    result <- curlGetString databaseLink []
    case result of
        (CurlOK, s) -> return s
        (err,_) -> error ("CURL_ERROR: " ++ show err)

parseRawDatabase :: String -> DeerTextDb
parseRawDatabase =
    lines >>>
    filter (not . null) >>>
    mapMaybe (parseLine >>> safePass) >>>
    filter censorShip >>>
    IM.fromList
  where
    -----------------    \ saluton! /
    parseLine = span (/= '.') -- ('.' =\)
    safePass :: (String,String) -> Maybe (Int,String)
    safePass (k,'.':' ':v)
      | any (null . snd) (reads k :: [(Int,String)]) = Just (read k,v)
    safePass _ = Nothing
    censorShip = fst >>> (/= 6)
