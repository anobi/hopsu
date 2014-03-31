module Hopsu.Db where

import Control.Monad.Trans
import Database.HDBC
import Database.HDBC.Sqlite3

data Db = Db {connection :: Connection}

runDb :: String -> IO Db
runDb db = do
    conn <- connectSqlite3 db
    return (Db conn)

geturl :: Connection -> String -> IO (Maybe String)
geturl c s = do 
    q <- quickQuery' c "SELECT url FROM urls WHERE name = ?" [toSql s]
    case q of
        [x] -> return $ Just $ convUrl x
        []  -> fail $ "no results" 
        x   -> fail $ "more than one result"

convUrl :: [SqlValue] -> String
convUrl [svId, svUrl] =
    show "" ++ fromSql svUrl
