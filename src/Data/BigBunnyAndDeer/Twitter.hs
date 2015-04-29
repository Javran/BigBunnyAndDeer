{-# LANGUAGE OverloadedStrings #-}
module Data.BigBunnyAndDeer.Twitter
  ( getOAuthTokens
  ) where

import Control.Applicative
import Web.Twitter.Conduit hiding (map,lookup)
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
