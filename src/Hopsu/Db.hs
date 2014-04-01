module Hopsu.Db where

import Database.HDBC
import Database.HDBC.Sqlite3

geturl :: Connection -> String -> IO (Maybe String)
geturl c s = handleSqlError $ do  
    q <- quickQuery' c "SELECT * FROM urls" []
    case q of
        ([id,n,url]:_) -> return $ Just $ fromSql n ++ " > " ++ fromSql url
        _ -> return $ Just $ "AAH" ++ show q

convUrl :: [SqlValue] -> String
convUrl [svId, svUrl] =
    show val
    where val = case fromSql svUrl of
            Just x -> x
            Nothing -> "nope"
convUrl x = fail $ "a failure is " ++ show x
