{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple (NamedParam(..))
import Database.SQLite.Simple.QQ
import Data.Int
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Client as HTTP
import Text.Printf
import Data.Foldable
import Control.Concurrent
import Control.Exception
import Data.Functor
import Control.Concurrent.STM
import Data.List
import Control.Monad

type XkcdNum = Int64

data Xkcd = Xkcd
  { xkcdNum :: XkcdNum
  , xkcdTitle :: T.Text
  , xkcdImg :: T.Text
  , xkcdAlt :: T.Text
  , xkcdTranscript :: T.Text
  } deriving (Eq, Show)

instance FromJSON Xkcd where
  parseJSON (Object v) =
    Xkcd <$> v .: "num" <*> v .: "title" <*> v .: "img" <*> v .: "alt" <*>
    v .: "transcript"
  parseJSON invalid = typeMismatch "Xkcd" invalid


openXkcdDatabase :: String -> IO Sqlite.Connection
openXkcdDatabase filePath = do
  dbConn <- Sqlite.open filePath
  Sqlite.executeNamed
    dbConn
    [sql|CREATE TABLE IF NOT EXISTS xkcd (
           num INTEGER UNIQUE,
           title TEXT,
           img TEXT,
           alt TEXT,
           transcript TEXT
         );|]
    []
  return dbConn

queryXkcdByURL :: HTTP.Manager -> String -> IO Xkcd
queryXkcdByURL manager url = do
  request <- HTTP.parseRequest url
  response <- HTTP.httpLbs request manager
  case eitherDecode $ HTTP.responseBody response of
    Right xkcd -> return xkcd
    Left errorMessage -> error errorMessage

queryCurrentXkcd :: HTTP.Manager -> IO Xkcd
queryCurrentXkcd manager =
  queryXkcdByURL manager "https://xkcd.com/info.0.json"

queryXkcdById :: HTTP.Manager -> XkcdNum -> IO Xkcd
queryXkcdById manager num =
  queryXkcdByURL manager $ printf "https://xkcd.com/%d/info.0.json" num

dumpXkcdToDb :: Xkcd -> Sqlite.Connection -> IO ()
dumpXkcdToDb Xkcd { xkcdNum = num
                  , xkcdTitle = title
                  , xkcdImg = img
                  , xkcdAlt = alt
                  , xkcdTranscript = transcript
                  } dbConn =
  Sqlite.executeNamed
    dbConn
    [sql|INSERT INTO xkcd (num, title, img, alt, transcript)
         VALUES (:num, :title, :img, :alt, :transcript)|]
    [ ":num" := num
    , ":title" := title
    , ":img" := img
    , ":alt" := alt
    , ":transcript" := transcript
    ]

instance Sqlite.FromRow Xkcd where
  fromRow =
    Xkcd <$> Sqlite.field <*> Sqlite.field <*> Sqlite.field <*> Sqlite.field <*>
    Sqlite.field

searchXkcdsInDbByContext :: Sqlite.Connection -> T.Text -> IO [Xkcd];
searchXkcdsInDbByContext dbConn context =
  Sqlite.queryNamed
    dbConn
    [sql|SELECT num, title, img, alt, transcript
         FROM xkcd
         WHERE transcript like '%' || :context || '%'|]
    [":context" := context]

scrapXkcdById :: HTTP.Manager -> Sqlite.Connection -> XkcdNum -> IO ()
scrapXkcdById manager dbConn num = do
  xkcd <- queryXkcdById manager num
  dumpXkcdToDb xkcd dbConn

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

databaseThread :: Sqlite.Connection -> TQueue Xkcd -> Int64 -> Int64 -> IO ()
databaseThread dbConn xkcdsQueue numCount numCap =
  when (numCount < numCap) $ do
    xkcds <- atomically $ flushTQueue xkcdsQueue
    Sqlite.withTransaction dbConn $
      for_ (zip [1 ..] xkcds) $ \(i, xkcd) -> do
        printf "Dumping xkcd %d/%d...\n" (numCount + i) numCap
        dumpXkcdToDb xkcd dbConn
    databaseThread dbConn xkcdsQueue (numCount + genericLength xkcds) numCap

downloaderThread :: HTTP.Manager -> TQueue Xkcd -> [XkcdNum] -> IO ()
downloaderThread manager xkcdsQueue nums = do
  for_ nums $ \num -> do
    xkcd <- queryXkcdById manager num
    atomically $ writeTQueue xkcdsQueue xkcd

main :: IO ()
main = do
  let chunkSize = 100
  manager <- TLS.newTlsManager
  dbConn <- openXkcdDatabase "database.db"
  current <- queryCurrentXkcd manager
  xkcdQueue <- atomically $ newTQueue
  let xkcdNums = filter (/= 404) [1 .. xkcdNum current]
  traverse (forkIO . downloaderThread manager xkcdQueue) (chunks chunkSize xkcdNums)
  databaseThread dbConn xkcdQueue 0 (genericLength xkcdNums)
