{-# LANGUAGE OverloadedStrings #-}
import Text.Printf
import Data.List
import Control.Arrow
import Data.Function
import Control.Monad.State
import Control.Applicative
import Control.Monad.Reader

import Data.BigBunnyAndDeer.DeerText
import Data.BigBunnyAndDeer.DeerInfo
import Data.BigBunnyAndDeer.Type
import Data.BigBunnyAndDeer.Util
import Data.BigBunnyAndDeer.Twitter
import Data.BigBunnyAndDeer.BigBunny

import Data.Random.RVar
import Data.Random.Shuffle.Weighted

import Data.Random
import System.Random.MWC (create)

pickNextDeer :: BigBunnyT IO (DeerId,DeerText)
pickNextDeer = do
    db <- ask
    di <- get
    -- WARNING: to get a complete set of DeerId,
    --   DO NOT use deerinfo as which might be incomplete
    -- frequency varies, so we give up its control
    let dis = allDeerIds db
        getLastTime = lastAccess . findDeerEntry di
        -- most recent tweets appear earlier, so we can drop them
        disRecentFirst = sortBy (flip compare `on` getLastTime) dis
        -- MAGIC: designed for posting 24 tweets a day,
        --   therefore set to a number slightly bigger:
        disCandidate = drop 30 disRecentFirst
        dfreqCandiate = map (findDeerEntry di >>> totalTime)
                            disCandidate
        -- add smooth to every frequency to ensure the sum isn't zero
        -- and make the distribution slightly more smooth
        smooth = 1
        adjustedFreqCandidate = map (+ smooth) dfreqCandiate
        adjustedSum = sum adjustedFreqCandidate
        adjustedProb = map (\fq -> adjustedSum - fq) adjustedFreqCandidate
        weightedCandidate = zip adjustedProb disCandidate

        chooseId :: RVar DeerId
        chooseId = head <$> weightedSample 1 weightedCandidate
    -- choose one entry
    pickId <- liftIO $ do
        mwc <- create
        sampleFrom mwc chooseId
    ts <- liftIO getCurrentTimestamp
    -- record choice
    modify (updateDeerInfo pickId ts)
    return (pickId, findDeerText db pickId)


main :: IO ()
main = do
    db <- fetchDatabase
    (did,dtext) <- runBigBunny db pickNextDeer
    let msg = printf "%d. %s\n" did dtext
    putStrLn ("Posting message: " ++ msg)
    postTweet msg
