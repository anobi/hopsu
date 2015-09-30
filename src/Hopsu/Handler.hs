module Hopsu.Handler where

import Data.List()
import Data.Maybe
import Database.HDBC.Sqlite3

import Hopsu.Types
import Hopsu.Db as DB
import Hopsu.Heather as Heather

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

weather :: String -> IO String
weather city = do
    w <- Heather.getWeather city
    case w of
        Just (Weather {}) -> return $ tellWeather $ fromJust w
        _ -> return "juuh en tiärä..."

tellWeather :: Weather -> String
tellWeather w =
    "Elikkäs " ++ description w ++ ", lämpöä " ++ show (temp w) ++ " astetta ja tuuleepi " ++ show (wSpeed w) ++ " m/s suunnasta " ++ show (wDeg w)
