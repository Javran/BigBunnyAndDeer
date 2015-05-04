{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Data.BigBunnyAndDeer.Type where

import Network.Curl
import Data.Default
import Control.Monad.State
import Control.Monad.Reader
import Data.Monoid
import qualified Data.IntMap as IM

type DeerId = Int
type DeerText = String
type DeerTextDb = IM.IntMap DeerText

data DeerEntry = DeerEntry
  { totalTime  :: Int
  , lastAccess :: Maybe Integer
  }

type DeerInfo = IM.IntMap DeerEntry

type BigBunnyT m a = StateT DeerInfo (ReaderT (DeerTextDb,BigBunnyConfig) m) a

data BigBunnyConfig = BigBunnyConfig
  { textDatabaseURL  :: URLString
  , authFilePath     :: FilePath
  , deerInfoFilePath :: FilePath
  }

instance Default BigBunnyConfig where
    def = BigBunnyConfig dbLink "auth.conf" "deerinfo.txt"
      where
        dbLink = "https://gist.githubusercontent.com/Javran/\
                 \8595d3587a8aa0c5255f/raw/gistfile1.md"

instance Default DeerEntry where
    def = DeerEntry def def

instance Monoid DeerEntry where
    mempty = def
    (DeerEntry t1 l1) `mappend` (DeerEntry t2 l2) =
        DeerEntry (t1+t2) (max l1 l2)

instance Monoid DeerInfo where
    mempty = def
    mappend = IM.unionWith (<>)
