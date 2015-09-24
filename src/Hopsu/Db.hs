module Hopsu.Db where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.ByteString.UTF8 as BS
import Data.Bool()
import Data.IORef

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

addOp :: Connection -> String -> String -> IO String
addOp db ident chan = do
    uid <- getUserId db ident
    chid <- getChanId db chan
    _ <- quickQuery db "UPDATE userchannel SET op = 1 WHERE user_id = ? AND channel_id = ?;" [toSql uid, toSql chid]
    commit db
    return $ "added " ++ ident ++ " as an op on " ++ chan

logUser :: Connection -> String -> String -> String -> IO String
logUser db nick ident chan
    | uid == 0 = return $ "logged user " ++ nick ++ " " ++ ident ++ " @ " ++ chan
    | uid >= 1 = addUser db ident nick chan
    | otherwise = return "error logging user"
    where
        uid = x >>= \x <- getUserId db ident
        chansUpdated = isOnChannel db ident chan
        nicksUpdated = hasNick db ident nick

addUser :: Connection -> String -> String -> String -> IO String
addUser db ident nick chan = do
    _ <- quickQuery db "INSERT INTO USERS (ident) VALUES (?);" [toSql ident]
    commit db
    addToChannel db ident chan
    addNick db ident nick
    return $ "added new user " ++ ident

addToChannel :: Connection -> String -> String -> IO ()
addToChannel db ident chan = do
    -- add to channel
    chid <- getChanId db chan
    uid <- getUserId db ident
    _ <- quickQuery db "INSERT INTO userchannel (user_id, channel_id, op, banned) \
                    \VALUES (?, ?, ?, ?);" [toSql uid, toSql chid, toSql "0", toSql "0"]
    commit db

addNick :: Connection -> String -> String -> IO ()
addNick db ident nick = do
    -- add nick to user
    uid <- getUserId db ident
    _ <- quickQuery db "INSERT INTO nick (user_id, nick) VALUES (?, ?);" [toSql uid, toSql nick]
    commit db

isOnChannel :: Connection -> String -> String -> IO String
isOnChannel db ident chan = do
    q <- quickQuery db "SELECT uc.channel_id FROM userchannel uc \
                       \JOIN users u ON u.user_id = uc.user_id \
                       \JOIN channels c ON c.channel_id = uc.channel_id \
                       \WHERE u.ident = ? AND c.channel = ?;" [toSql ident, toSql chan]
    case q of
        [[SqlByteString _]] -> return ""
        _ -> do 
            addToChannel db ident chan
            return $ "added " ++ ident ++ " to channel " ++ chan

hasNick :: Connection -> String -> String -> IO String
hasNick db ident nick = do
    q <- quickQuery db "SELECT nick_id FROM nick n \
                        \ JOIN users u ON u.user_id = n.user_id \
                        \ WHERE u.ident = ? AND n.nick = ?;" [toSql ident, toSql nick]
    case q of
        [[SqlByteString _]] -> return ""
        _ -> do
            addNick db ident nick
            return $ "updated " ++ ident ++ " with nick " ++ nick

getUserId :: Connection -> String -> IO Int
getUserId db ident = do
    uid <- quickQuery db "SELECT user_id FROM users WHERE ident = ?;" [toSql ident]
    return $ fromSql $ head uid !! 0

getChanId :: Connection -> String -> IO Int
getChanId db chan = do
    chid <- quickQuery db "SELECT channel_id FROM channels WHERE channel = ?;" [toSql chan]
    return $ fromSql $ head chid !! 0
    
sqlToBool :: String -> Bool
sqlToBool s
    | s == "0" = False
    | s == "1" = True
    | otherwise = False

