module Main where

{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import System.Environment
import System.IO
import Network.Curl
import Text.Regex.Posix
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import Data.Aeson

Data Finance = Finance {
    id :: Text,
    t :: Text,
    e :: Text,
    l :: Text,
    l_fix :: Double,
    l_cur :: Text,
    s :: Text,
    ltt :: Text,
    lt :: Text,
    lt_dts :: Text,
    c :: Double,
    c_fix :: Double,
    cp :: Double,
    cp_fix :: Double,
    ccol :: Text,
    pcls_fix :: Double
} deriving (Generic, Show)

instance FromJSON Finance
instance ToJSON Finance

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
