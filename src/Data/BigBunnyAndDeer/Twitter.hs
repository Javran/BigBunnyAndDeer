{-# LANGUAGE OverloadedStrings #-}
module Data.BigBunnyAndDeer.Twitter
  ( postTweet
  ) where

import qualified Data.Text as T
import Web.Twitter.Conduit hiding (map,lookup)
import qualified Data.ByteString.Char8 as S8
import Data.Either.Utils
import Data.ConfigFile

getOAuthTokens :: FilePath -> IO (OAuth, Credential)
getOAuthTokens fp = do
    cfgVal <- readfile emptyCP { optionxform = id } fp
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

postTweet :: FilePath -> String -> IO ()
postTweet fp msg = do
    twInfo <- getTWInfoFromEnv fp
    mgr <- newManager tlsManagerSettings
    print =<< call twInfo mgr (update (T.pack msg))

getTWInfoFromEnv :: FilePath -> IO TWInfo
getTWInfoFromEnv fp = do
    (oa, cred) <- getOAuthTokens fp
    return $ (setCredential oa cred def) { twProxy = Nothing }
