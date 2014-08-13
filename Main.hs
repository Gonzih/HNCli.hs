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
import System.IO (stderr, hPutStrLn, hPrint)
import qualified Data.Foldable as F

data Item = Item { title        :: String
                 , url          :: String
                 , id           :: Maybe Int
                 , commentCount :: Maybe Int
                 , points       :: Maybe Int
                 , postedAgo    :: Maybe String
                 , atuhor       :: Maybe String
                 } deriving (Show)

data Feed = Feed { items :: [Item]
                 } deriving (Show)

instance FromJSON Item where
    parseJSON (Object v) = Item
                           <$> v .: "title"
                           <*> v .: "url"
                           <*> v .: "id"
                           <*> v .: "commentCount"
                           <*> v .: "points"
                           <*> v .: "postedAgo"
                           <*> v .: "author"

    parseJSON _          = mzero

instance FromJSON Feed where
    parseJSON (Object v) = Feed
                           <$> v .: "items"

    parseJSON _          = mzero

statusExceptionHandler ::  HttpException -> IO L.ByteString
statusExceptionHandler (StatusCodeException status _ _) =
    hPutStrLn stderr "An error occured during download: "
    >> hPrint stderr status
    >> return L.empty
statusExceptionHandler exception =
    hPutStrLn stderr "An error occured during download: "
    >> hPrint stderr exception
    >> return L.empty

jsonData :: IO L.ByteString
jsonData = simpleHttp "http://hn.gonzih.me/" `catch` statusExceptionHandler

numberFromMaybe :: Maybe Int -> Int
numberFromMaybe (Just n) = n
numberFromMaybe Nothing = 0

getIntWith :: (Item -> Maybe Int) -> Item -> Int
getIntWith fn = numberFromMaybe . fn

formattedLine :: Item -> String
formattedLine = liftM4 (printf "\n%-3d (%-3d) %s\n          %s\n") getPoints getComments title url
                where getPoints   = getIntWith points
                      getComments = getIntWith commentCount

lowercasedTitle :: Item -> String
lowercasedTitle = map toLower . title

interestingKeywords :: [String]
interestingKeywords  = [ "haskell"
                       , "clojure"
                       , "arduino"
                       , "raspberry"
                       ]

isInteresting :: Item -> Bool
isInteresting item  = any (`isInfixOf` lowercasedTitle item) interestingKeywords

formatFeed :: Feed -> String
formatFeed = concatMap formattedLine . filter isInteresting . items

main :: IO ()
main = F.mapM_ (putStr . formatFeed) . decode =<< jsonData
