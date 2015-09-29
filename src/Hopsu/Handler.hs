module Hopsu.Handler where

import Data.List()
import Database.HDBC.Sqlite3
import Control.Monad.Reader
import Control.Monad (when)

import Hopsu.Types
import Hopsu.Db as DB

--
-- todo: here be the functionality, the good stuff
--

-- ok this kind of thing (inputs, millions of strings) is getting pretty shitty
-- gotta create an user object or something and use that as an input
-- but that's a refactoring battle for another day
-- so ANYWAY this function checks if joiner already exists in users, add if not
-- update new nick if such things are necessary
logUser :: Connection -> User -> IO String
logUser db user = liftIO $ DB.logUser db (nick user) (ident user) (chan user)
    
op :: Connection -> User -> IO (Maybe IrcMessage)
op db user = do
    o <- liftIO $ DB.isOp db (ident user) (chan user)
    if o then return IrcMessage { command = "MODE", params = opstring }
    else return Nothing
    where opstring = chan user ++ " +o " ++ nick user 

getUrl :: Connection -> String -> IO String
getUrl = DB.geturl

newurl :: Connection -> Url -> IO String 
newurl db u = DB.addurl db (alias u) (url u)
