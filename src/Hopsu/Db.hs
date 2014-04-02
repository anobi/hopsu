module Hopsu.Db where

import Database.HDBC
import Database.HDBC.Sqlite3

geturl :: Connection -> String -> IO String
geturl c s = handleSqlError $ do  
    q <- quickQuery' c "SELECT * FROM urls" []
    case q of
        ([id,n,url]:_) -> return $ fromSql url
        _ -> return "No."
