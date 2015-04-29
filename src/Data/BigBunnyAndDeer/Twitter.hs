{-# LANGUAGE OverloadedStrings #-}
module Data.BigBunnyAndDeer.Twitter
  ( postTweet
  ) where

import Control.Applicative
import qualified Data.Text as T
import Web.Twitter.Conduit hiding (map,lookup)
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import qualified Data.ByteString.Char8 as S8
import Data.Either.Utils
import Data.ConfigFile

getOAuthTokens :: IO (OAuth, Credential)
getOAuthTokens = do
    cfgVal <- readfile emptyCP { optionxform = id } "auth.conf"
    let cp = forceEither cfgVal
        getConf = S8.pack <$> forceEither . get cp "DEFAULT"
        consumerKey    = getConf "OAUTH_CONSUMER_KEY"
        consumerSecret = getConf "OAUTH_CONSUMER_SECRET"
        accessToken    = getConf "OAUTH_ACCESS_TOKEN"
        accessSecret   = getConf "OAUTH_ACCESS_SECRET"
        oauth = twitterOAuth
            { oauthConsumerKey = consumerKey
            , oauthConsumerSecret = consumerSecret
            }
        cred = Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessSecret)
            ]
    return (oauth, cred)

postTweet :: String -> IO ()
postTweet msg = do
    twInfo <- getTWInfoFromEnv
    print =<< withManager (\mgr -> call twInfo mgr $ update (T.pack msg))

getTWInfoFromEnv :: IO TWInfo
getTWInfoFromEnv = do
    (oa, cred) <- getOAuthTokens
    return $ (setCredential oa cred def) { twProxy = Nothing }
