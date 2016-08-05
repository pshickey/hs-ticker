module Main where

import Network.Curl
import Text.Regex.Posix

main :: IO ()
main = withCurlDo $ do
    let url = "https://www.google.com/finance?q=INDEXNASDAQ%3A.IXIC"
        dataPattern = "<meta itemprop=\".*\"\n *content=\".*\" />"
    (_, page) <- curlGetString url [CurlTimeout 30]
    let matches = map concat (page =~ dataPattern :: [[String]])
        info    = extractData matches
            where
                extractData = map (\x -> (extractLabel x, extractVal x))
                extractLabel = getQuoteField
                extractVal = reverse . getQuoteField . reverse
                getQuoteField = (takeWhile (/='"') . tail . dropWhile (/='"'))
    mapM_ print info