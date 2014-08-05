{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, liftM4)
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Text.Printf (printf)
import Data.List (isInfixOf)
import Data.Char (toLower)
import Control.Exception (catch)
import System.IO (stderr, hPutStrLn)

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

statusExceptionHandler ::  HttpException -> IO L.ByteString
statusExceptionHandler (StatusCodeException status _ _) =
    hPutStrLn stderr "An error occured during download: "
    >> print status
    >> return L.empty
statusExceptionHandler _ =
    hPutStrLn stderr "An error occured during download (unhandled)"
    >> return L.empty

jsonData :: IO L.ByteString
jsonData = simpleHttp "http://api.ihackernews.com/page" `catch` statusExceptionHandler

main :: IO ()
main = do
    string <- jsonData
    let feed = decode string :: Maybe Feed
    case feed of
      Just parsedFeed -> mapM_ (putStrLn . formattedLine) $ filter isInteresting $ items parsedFeed
      Nothing         -> return ()
    where formattedLine       = liftM4 (printf "%-3d (%-3d) %s\n          %s\n") points commentCount title url
          isInteresting item  = or [keyword `isInfixOf` map toLower (title item) | keyword <- interestingKeywords]
          interestingKeywords = [ "haskell"
                                , "clojure"
                                , "arduino"
                                , "raspberry"
                                ]
