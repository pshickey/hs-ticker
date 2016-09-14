{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import GHC.Generics
import System.Environment
import System.IO
import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8
import Network.HTTP.Conduit (simpleHttp)

data Finance =
    Finance {
        id :: Text,
        t :: Text,
        e :: Text,
        l :: Text,
        l_fix :: Text,
        l_cur :: Text,
        s :: Text,
        ltt :: Text,
        lt :: Text,
        lt_dts :: Text,
        c :: Text,
        c_fix :: Text,
        cp :: Text,
        cp_fix :: Text,
        ccol :: Text,
        pcls_fix :: Text
    } deriving (Generic, Show)

instance FromJSON Finance
instance ToJSON Finance

main :: IO()
main = do
    [stock_file] <- getArgs
    handle <- openFile stock_file ReadMode
    txt <- hGetContents handle
    let queryBase = "https://www.google.com/finance/info?q="
        query = queryBase ++ txt
    json_str <- simpleHttp query
    let clean_json = C8.dropWhile (/='[') json_str
        objs = eitherDecode clean_json :: Either String [Finance]
    case objs of
        Left s -> putStrLn s
        Right f -> print f

{-
import System.Environment
import System.IO
import Network.Curl
import Text.Regex.Posix
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe

main :: IO ()
main = withCurlDo $ do
    -- The name of the file containing stock symbols should be the only arg
    -- TODO: case for safely handling command line
    [stock_file] <- getArgs
    handle <- openFile stock_file ReadMode
    txt <- hGetContents handle
    let stocks      = lines txt
        urlBase     = "https://www.google.com/finance?q="
        urls        = map (urlBase++) stocks
        dataPattern = "<meta itemprop=\".*\"\n *content=\".*\" />"
    pages <- forM urls (\x -> do
            (_, html) <- curlGetString x [CurlTimeout 30]
            return html)
    hClose handle

    let matches = map findMatches pages
            where
                findMatches :: [Char] -> [[Char]]
                findMatches s = map concat (s =~ dataPattern :: [[String]])
        allData = map extractData matches
            where
                extractData   = map (\x -> (extractLabel x, extractVal x))
                extractLabel  = getQuoteField
                extractVal    = reverse . getQuoteField . reverse
                getQuoteField = takeWhile (/='"') . tail . dropWhile (/='"')
        relevant = map filterData allData
            where
                fields = ["name", "tickerSymbol", "price", "priceChange", "priceChangePercent"]
                filterData xs = mapMaybe (\s -> lookup s xs) fields
    mapM_ prettyPrint relevant
        where
            prettyPrint [name, sym, price, change, percent] =
                putStrLn $
                    name ++ " --- ("++ sym ++ ")\n\t" ++
                        show (price, change, percent)
            prettyPrint _  = putStrLn "error"
-}