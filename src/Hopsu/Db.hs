module Hopsu.Db where

import Database.HDBC.Sqlite3

data Db = Db {connection :: Connection}

runDb :: String -> IO Db
runDb db = do
    conn <- connectSqlite3 db
    return (Db conn)
