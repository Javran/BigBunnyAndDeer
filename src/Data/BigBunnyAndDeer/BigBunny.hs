module Data.BigBunnyAndDeer.BigBunny
  ( runBigBunny
  , getTextDb
  , getDeerInfo
  , getTextDatabaseURL
  , getAuthFilePath
  , getDeerInfoFilePath
  ) where

import Control.Monad.State
import Control.Monad.Reader

import Data.BigBunnyAndDeer.DeerInfo
import Data.BigBunnyAndDeer.DeerText
import Data.BigBunnyAndDeer.Type

runBigBunny :: MonadIO m => BigBunnyConfig -> BigBunnyT m a -> m a
runBigBunny conf m = do
    let diFilePath = deerInfoFilePath conf
    db <- liftIO $ fetchDatabase (textDatabaseURL conf)
    di <- liftIO $ fetchDeerInfo diFilePath
    (v,newDi) <- runReaderT (runStateT m di) (db,conf)
    liftIO $ writeDeerInfo diFilePath newDi
    return v

getTextDb :: Monad m => BigBunnyT m DeerTextDb
getTextDb = asks fst

getDeerInfo :: Monad m => BigBunnyT m DeerInfo
getDeerInfo = get

getTextDatabaseURL :: Monad m => BigBunnyT m String
getTextDatabaseURL = asks (textDatabaseURL . snd)

getAuthFilePath :: Monad m => BigBunnyT m FilePath
getAuthFilePath = asks (authFilePath . snd)

getDeerInfoFilePath :: Monad m => BigBunnyT m FilePath
getDeerInfoFilePath = asks (deerInfoFilePath . snd)
