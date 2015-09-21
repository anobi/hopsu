module Hopsu.Db where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.ByteString.UTF8 as BS
import Data.Bool()
import Debug.Trace

geturl :: Connection -> String -> IO String
geturl c s = handleSqlError $ do
    q <- quickQuery c "SELECT url FROM urls WHERE alias = ?;" [toSql s]
    case q of
        [[SqlByteString url]] -> return $ BS.toString url
        _ -> return ""

addurl :: Connection -> String -> String -> IO String
addurl c a u = do
    _ <- quickQuery c "INSERT INTO urls (alias, url) VALUES (?, ?);" [toSql a, toSql u]
    commit c
    return "ju"

isOp :: Connection -> String -> String -> IO Bool
isOp db ident chan = do
    q <- quickQuery db "SELECT op FROM userchannel uc \
                        \ JOIN users u ON u.user_id = uc.user_id \
                        \ JOIN channels c ON c.channel_id = uc.channel_id \
                        \ WHERE u.ident = ? AND c.channel = ?;" [toSql ident, toSql chan]
    case q of
        [[SqlByteString o]] -> return $ sqlToBool $ BS.toString o
        _ -> return False

sqlToBool :: String -> Bool
sqlToBool s
    | s == "0" = False
    | s == "1" = True
    | otherwise = False

