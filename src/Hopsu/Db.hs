module Hopsu.Db where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.ByteString.UTF8 as BS

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
