{-# LANGUAGE OverloadedStrings #-}
import Text.Printf
import Data.List
import Data.Function
import qualified Data.IntMap as IM

import Data.BigBunnyAndDeer.DeerText
import Data.BigBunnyAndDeer.DeerInfo
import Data.BigBunnyAndDeer.Type
import Data.BigBunnyAndDeer.Util
import Data.BigBunnyAndDeer.Twitter

pickNextDeer :: DeerTextDb -> IO DeerId
pickNextDeer db = do
    -- WARNING: to get a complete set of DeerId,
    --   DO NOT use deerinfo as which might be incomplete
    -- frequency varies, so we give up its control
    di <- getDeerInfo
    let dis = IM.keys db
        getLastTime = lastAccess . findDeerEntry di
        -- most recent tweets appear earlier, so we can drop them
        disRecentFirst = sortBy (flip compare `on` getLastTime) dis
        disCandidate = drop 30 disRecentFirst
    -- choose one entry
    pickId <- choice disCandidate
    -- record choice
    updateFreqInfo pickId
    return pickId

main :: IO ()
main = do
    db <- fetchDatabase
    did <- pickNextDeer db
    let msg = printf "%d. %s\n" did (findDeerText db did)
    putStrLn ("Posting message: " ++ msg)
    postTweet msg
