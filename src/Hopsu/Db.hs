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

logUser :: Connection -> String -> String -> String -> IO String
logUser db nick ident chan = do
    -- check if ident exists
    user <- quickQuery db "SELECT user_id FROM users WHERE ident = ?;" [toSql ident]
    -- something returns if the user exists, else we'll just add a new one
    case user of
        --User exists, update user's channels & nicks if necessary
        [[SqlByteString u]] -> do
            chansUpdated <- isOnChannel db ident chan
            nicksUpdated <- hasNick db ident nick
            return "user existed"
        -- User didn't exist, add new user & update channels and nick
        _ -> do
            quickQuery db "INSERT INTO USERS (ident) VALUES (?);" [toSql ident]
            commit db
            addToChannel db ident chan
            addNick db ident nick
            return "created new user"

isOnChannel :: Connection -> String -> String -> IO Bool
isOnChannel db ident chan = do
    q <- quickQuery db "SELECT channel_id FROM userchannel uc \
                        \ JOIN users u ON u.user_id = uc.user_id \
                        \ JOIN channels c ON c.channel_id = uc.channel_id \
                        \ WHERE u.ident = ? AND c.channel = ?;" [toSql ident, toSql chan]
    case q of
        [[SqlByteString _]] -> return True
        _ -> do
            addToChannel db ident chan
            return False

addToChannel :: Connection -> String -> String -> IO ()
addToChannel db ident chan = do
    -- add to channel
    chid <- quickQuery db "SELECT channel_id FROM channels WHERE channel = ?;" [toSql chan]
    uid <- quickQuery db "SELECT user_id FROM users WHERE ident = ?;" [toSql ident]
    quickQuery db "INSERT INTO userchannel (user_id, channel_id, op, banned) \
                    \VALUES (?, ?, ?, ?);" [toSql $ head uid !! 0, toSql $ head chid !! 0, toSql "0", toSql "0"]
    commit db
    return ()

hasNick :: Connection -> String -> String -> IO Bool
hasNick db ident nick = do
    q <- quickQuery db "SELECT nick_id FROM nick n \
                        \ JOIN users u ON u.user_id = n.user_id \
                        \ WHERE u.ident = ? AND n.nick = ?;" [toSql ident, toSql nick]
    case q of
        [[SqlByteString _]] -> return True
        _ -> do
            addNick db ident nick
            return False

addNick :: Connection -> String -> String -> IO ()
addNick db ident nick = do
    -- add nick to user
    uid <- quickQuery db "SELECT user_id FROM users WHERE ident = ?;" [toSql ident]
    quickQuery db "INSERT INTO nick (user_id, nick) VALUES (?, ?);" [toSql (head uid !! 0), toSql nick]
    commit db
    return ()

sqlToBool :: String -> Bool
sqlToBool s
    | s == "0" = False
    | s == "1" = True
    | otherwise = False

