module Hopsu.Db where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.ByteString.UTF8 as BS
import Data.Bool()

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

isOp :: Connection -> String -> IO Bool
isOp c s = do
    q <- quickQuery c "SELECT op FROM users WHERE nick = ?;" [toSql s]
    case q of
        [[SqlByteString isOp]] -> return $ sqlToBool $ BS.toString isOp
        _ -> return False

sqlToBool :: String -> Bool
sqlToBool s
    | s == "0" = False
    | s == "1" = True
    | otherwise = False
                 
