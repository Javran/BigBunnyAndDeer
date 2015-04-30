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

allDeerIds :: DeerTextDb -> [DeerId]
allDeerIds = IM.keys

lookupDeerText :: DeerTextDb -> DeerId -> Maybe DeerText
lookupDeerText = flip IM.lookup

findDeerText :: DeerTextDb -> DeerId -> DeerText
findDeerText db did = fromJust (lookupDeerText db did)

fetchDatabase :: URLString -> IO DeerTextDb
fetchDatabase url = parseRawDatabase <$> fetchRawDatabase url

fetchRawDatabase :: URLString -> IO String
fetchRawDatabase url = do
    result <- curlGetString url []
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
