{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import GHC.Generics
import Control.Monad
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
  stocks <- getArgs
  mapM_ gFinancePrint stocks

gFinancePrint f = do
  handle <- openFile f ReadMode
  txt <- hGetContents handle
  let
    queryBase = "https://www.google.com/finance/info?q="
    query = queryBase ++ txt
  json_str <- simpleHttp query
  let
    clean_json = C8.dropWhile (/='[') json_str
    objs = eitherDecode clean_json :: Either String [Finance]
  case objs of
    Left s -> putStrLn s
    Right f -> mapM_ (putStrLn . fmtFinance) f

fmtFinance (Finance _ t e _ _ l_cur _ _ lt _ c _ cp _ _ _) =
  unpack $
  Data.Text.unwords
    [t, e, "\n  Price: ", l_cur, lt, "\n  Change:", c, ",", (Data.Text.append cp "%")]
