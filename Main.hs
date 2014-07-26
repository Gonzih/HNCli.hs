{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Network.HTTP.Conduit
import Data.ByteString.Lazy.Internal (ByteString(..))
import Text.Printf (printf)

data Item = Item { title        :: String
                 , url          :: String
                 , id           :: Float
                 , commentCount :: Int
                 , points       :: Int
                 , postedAgo    :: Maybe String
                 , postedBy     :: Maybe String
                 } deriving (Show)

data Feed = Feed { nextId      :: Maybe String
                 , items       :: [Item]
                 , version     :: String
                 , cachedOnUTC :: String
                 } deriving (Show)

instance FromJSON Item where
    parseJSON (Object v) = Item
                           <$> v .: "title"
                           <*> v .: "url"
                           <*> v .: "id"
                           <*> v .: "commentCount"
                           <*> v .: "points"
                           <*> v .: "postedAgo"
                           <*> v .: "postedBy"

    parseJSON _          = mzero

instance FromJSON Feed where
    parseJSON (Object v) = Feed
                           <$> v .: "nextId"
                           <*> v .: "items"
                           <*> v .: "version"
                           <*> v .: "cachedOnUTC"

    parseJSON _          = mzero

jsonData :: IO Data.ByteString.Lazy.Internal.ByteString
jsonData = simpleHttp "http://api.ihackernews.com/page"

main :: IO ()
main = do
    string <- jsonData
    let feed = decode string :: Maybe Feed
    case feed of
      Just parsedFeed -> mapM_ (putStrLn . formattedLine) $ items parsedFeed
      a -> print a
    where formattedLine item = printf "%-3d (%-3d) %s\n          %s\n" (points item) (commentCount item) (title item) (url item)
