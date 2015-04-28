{-# LANGUAGE OverloadedStrings #-}
import Network.Curl
import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.Time
import System.Directory
import Text.Printf
import Control.Monad.Random
import Data.List
import Data.Monoid
import Data.Function
import qualified System.IO.Strict as SIO
import qualified Data.Text as T
import Web.Twitter.Conduit hiding (map,lookup)
import Network.HTTP.Conduit hiding (lookup)
import Web.Authenticate.OAuth
import System.Environment
import qualified Data.ByteString.Char8 as S8

type DeerId = Int
type DeerFreqEntry = ( Int -- total times posted
                     , Maybe Integer -- last access time in UTC
                     )

type DeerFreqInfo = [(DeerId,DeerFreqEntry)]
type DeerDb = [(DeerId,String)]

-- TODO: findDeer/findDeerEntry
findDeer :: DeerFreqInfo -> DeerId -> DeerFreqEntry
findDeer dfi k = fromMaybe (0,Nothing) (lookup k dfi)

findDeerQuote :: DeerDb -> DeerId -> String
findDeerQuote db k = fromJust (lookup k db)

deerInfo :: FilePath
deerInfo = "deerinfo.txt"

deerFreqInfo :: IO DeerFreqInfo
deerFreqInfo = do
    e <- doesFileExist deerInfo
    if e
       then read <$> SIO.readFile deerInfo
       else return []

updateFreqInfo :: DeerId -> IO ()
updateFreqInfo did = do
    fi <- deerFreqInfo
    ct <- getCurrentTimestamp
    let (count, _) = findDeer fi did
        entry = (did, (succ count, Just ct))
        newFI = (entry :) -- :)
              . filter ((/= did) . fst)
              $ fi
    writeFile deerInfo (show newFI)

getCurrentTimestamp :: IO Integer
getCurrentTimestamp = utcToInteger <$> getCurrentTime
  where
    tZero = UTCTime (ModifiedJulianDay 0) 0
    -- get millseconds for better precision
    utcToInteger :: UTCTime -> Integer
    utcToInteger = truncate . (* 1000) . (`diffUTCTime` tZero)

deerLink :: URLString
deerLink = "https://gist.githubusercontent.com/Javran/\
             \8595d3587a8aa0c5255f/raw/gistfile1.md"

getDeerRaw :: IO String
getDeerRaw = do
    result <- curlGetString deerLink []
    case result of
        (CurlOK, s) -> return s
        (err,_) -> error (show err)

parseDeer :: String -> DeerDb
parseDeer =
    lines >>>
    filter (not . null) >>>
    mapMaybe (parseLine >>> safePass) >>>
    filter censorShip
  where
    parseLine = span (/= '.')
    safePass :: (String,String) -> Maybe (Int,String)
    safePass (k,'.':' ':v)
      | any (null . snd) (reads k :: [(Int,String)]) = Just (read k,v)
    safePass _ = Nothing
    censorShip = fst >>> (/= 6)

pickNextDeer :: DeerDb -> IO DeerId
pickNextDeer db = do
    -- frequency varies, so we give up its control.
    fi <- deerFreqInfo
    let getLastTime = snd . findDeer fi
        getDeerFreq = fst . findDeer fi
        cmp d1 d2 = (compare `on` getLastTime) d1 d2 -- no history / early history first
                 <> (compare `on` getDeerFreq) d1 d2 -- frequency
        ids = sortBy cmp (map fst db)
    -- choose one entry
    pickId <- (ids !!) <$> getRandomR (0, length ids-1)
    -- record choice
    updateFreqInfo pickId
    return pickId

main :: IO ()
main = do
    db <- parseDeer <$> getDeerRaw
    did <- pickNextDeer db
    let msg = printf "%d. %s\n" did (findDeerQuote db did)
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
