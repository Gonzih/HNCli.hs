{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Network.HTTP.Conduit
import Data.ByteString.Lazy.Internal (ByteString(..))
import Text.Printf (printf)
import Data.List (isInfixOf)
import Data.Char (toLower)
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)

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

jsonData :: IO (Maybe Data.ByteString.Lazy.Internal.ByteString)
jsonData = runMaybeT $ simpleHttp "http://api.ihackernews.com/page"

main :: IO ()
main = do
    maybeString <- jsonData
    let string = fromMaybe "" maybeString
        feed   = decode string :: Maybe Feed
    case feed of
      Just parsedFeed -> mapM_ (putStrLn . formattedLine) $ filter isInteresting $ items parsedFeed
      Nothing         -> return ()
    where formattedLine item  = printf "%-3d (%-3d) %s\n          %s\n" (points item) (commentCount item) (title item) (url item)
          isInteresting item  = or [bool `isInfixOf` map toLower (title item) | bool <- interestingKeywords]
          interestingKeywords = [ "haskell"
                                , "clojure"
                                , "arduino"
                                , "raspberry"
                                ]
