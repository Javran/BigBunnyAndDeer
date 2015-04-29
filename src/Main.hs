{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Text.Printf
import Control.Monad.Random
import Data.List
import Data.Monoid
import Data.Function
import qualified Data.Text as T
import Web.Twitter.Conduit hiding (map,lookup)
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import System.Environment
import qualified Data.ByteString.Char8 as S8
import qualified Data.IntMap as IM

import Data.BigBunnyAndDeer.DeerText
import Data.BigBunnyAndDeer.DeerInfo
import Data.BigBunnyAndDeer.Type

pickNextDeer :: DeerTextDb -> IO DeerId
pickNextDeer db = do
    -- frequency varies, so we give up its control.
    fi <- getDeerInfo
    let getLastTime = lastAccess . findDeerEntry fi
        getDeerFreq = totalTime . findDeerEntry fi
        cmp d1 d2 = (compare `on` getLastTime) d1 d2 -- no history / early history first
                 <> (compare `on` getDeerFreq) d1 d2 -- frequency
        ids = sortBy cmp (map fst (IM.toList db))
    -- choose one entry
    pickId <- (ids !!) <$> getRandomR (0, length ids-1)
    -- record choice
    updateFreqInfo pickId
    return pickId

main :: IO ()
main = do
    db <- fetchDatabase
    did <- pickNextDeer db
    let msg = printf "%d. %s\n" did (findDeerText db did)
    putStrLn ("Posting message: " ++ msg)
    twInfo <- getTWInfoFromEnv
    print =<< withManager (\mgr -> call twInfo mgr $ update (T.pack msg))

getOAuthTokens :: IO (OAuth, Credential)
getOAuthTokens = do
    consumerKey <- getEnv' "OAUTH_CONSUMER_KEY"
    consumerSecret <- getEnv' "OAUTH_CONSUMER_SECRET"
    accessToken <- getEnv' "OAUTH_ACCESS_TOKEN"
    accessSecret <- getEnv' "OAUTH_ACCESS_SECRET"
    let oauth = twitterOAuth
            { oauthConsumerKey = consumerKey
            , oauthConsumerSecret = consumerSecret
            }
        cred = Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessSecret)
            ]
    return (oauth, cred)
  where
    getEnv' = (S8.pack <$>) . getEnv

getTWInfoFromEnv :: IO TWInfo
getTWInfoFromEnv = do
    (oa, cred) <- getOAuthTokens
    return $ (setCredential oa cred def) { twProxy = Nothing }
