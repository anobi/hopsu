module Hopsu.Db where

import Database.HDBC
import Database.HDBC.Sqlite3

geturl :: Connection -> String -> IO (Maybe String)
geturl c s = handleSqlError $ do  
    q <- quickQuery' c "SELECT url FROM urls WHERE name = ?" [toSql s]
    case q of
        [x] -> return $ Just $ convUrl x
        [] -> return $ Just "nope"
        x -> return $ Just "nope"

convUrl :: [SqlValue] -> String
convUrl [svId, svUrl] =
    show "" ++ fromSql svUrl
