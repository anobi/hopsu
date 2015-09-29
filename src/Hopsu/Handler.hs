module Hopsu.Handler where

import Data.List()
import Database.HDBC.Sqlite3

import Hopsu.Types
import Hopsu.Db as DB

-- checks if joiner already exists in users, add if not
-- update new nick if such things are necessary
logUser :: Connection -> User -> IO String
logUser c user = DB.logUser c (nick user) (ident user) (chan user)
    
op :: Connection -> User -> IO (Maybe IrcMessage)
op c user = do
    o <- DB.isOp c (ident user) (chan user)
    if o then return $ Just IrcMessage { command = "MODE", params = opstring }
    else return Nothing
    where opstring = chan user ++ " +o " ++ nick user 

getUrl :: Connection -> String -> IO String
getUrl = DB.geturl

newurl :: Connection -> Url -> IO String 
newurl c u = DB.addurl c (alias u) (url u)
