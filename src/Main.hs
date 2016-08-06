module Main where

import System.IO
import Network.Curl
import Text.Regex.Posix
import Control.Monad

main :: IO ()
main = withCurlDo $ do
    handle <- openFile "stocks.txt" ReadMode
    txt <- hGetContents handle
    let stocks = lines txt
        urlBase = "https://www.google.com/finance?q="
        urls = map (urlBase++) stocks
        dataPattern = "<meta itemprop=\".*\"\n *content=\".*\" />"
    pages <- forM urls (\x -> do
            (_, html) <- curlGetString x [CurlTimeout 30]
            return html)
    let matches = map findMatches pages
            where
                findMatches :: [Char] -> [[Char]]
                findMatches s = map concat (s =~ dataPattern :: [[String]])
        info = map extractData matches
            where
                extractData   = map (\x -> (extractLabel x, extractVal x))
                extractLabel  = getQuoteField
                extractVal    = reverse . getQuoteField . reverse
                getQuoteField = takeWhile (/='"') . tail . dropWhile (/='"')
    hClose handle
    mapM_ print info