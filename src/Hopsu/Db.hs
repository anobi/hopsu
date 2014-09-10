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

addurl :: Connection -> [String] -> IO String
addurl c s = handleSqlError $ do  
    _ <- quickQuery c "INSERT INTO url (alias, url) VALUES (?, ?);" [toSql (s !! 0), toSql (s !! 1)]
    commit c
    return "ju"
