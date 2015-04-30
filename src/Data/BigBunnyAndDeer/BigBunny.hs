module Data.BigBunnyAndDeer.BigBunny
  ( BigBunnyT
  , runBigBunny
  ) where

import Control.Monad.State
import Control.Monad.Reader

import Data.BigBunnyAndDeer.DeerInfo
import Data.BigBunnyAndDeer.DeerText

type BigBunnyT m a = StateT DeerInfo (ReaderT DeerTextDb m) a

runBigBunny :: DeerTextDb -> BigBunnyT IO a -> IO a
runBigBunny db m = do
    di <- getDeerInfo
    (v,newDi) <- runReaderT (runStateT m di) db
    writeDeerInfo newDi
    return v
