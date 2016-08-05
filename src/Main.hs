module Main where

import Network.Curl
import Text.Regex.Posix

main :: IO ()
main = withCurlDo $ do
    let url = "https://www.google.com/finance?q=INDEXNASDAQ%3A.IXIC"
        dataPattern = "<meta itemprop=\".*\"\n *content=\".*\" />"
    (_, page) <- curlGetString url [CurlTimeout 10]
    let matches = map concat (page =~ dataPattern :: [[String]])
        info    = extractData matches
            where
                extractData = map (\x -> (extractLabel x, extractVal x))
                extractLabel = getQuoteField
                extractVal = reverse . getQuoteField . reverse
                getQuoteField = (takeWhile (/='"') . tail . dropWhile (/='"'))
    -- info is pretty much an assoc list so I can write a function to lookup fields I actually care about
    mapM_ print info
