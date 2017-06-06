module Hopsu.Handler where

import Data.List()

-- tell the uptime
uptime :: Net String
uptime = do
    now <- liftIO getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

-- say hello to the guy who just joined the channel
greet :: String -> Net ()
greet x =
    privmsg $ "Eyh " ++ guy ++ ", " ++ greeting
    where
        guy = takeWhile (/= '!') x
        greeting = "loool"


--
-- Users
--
logUser :: String -> String -> String -> Net()
logUser nick ident chan = do
  consoleWrite "logging user??"
  conn <- asks db
  result <- liftIO $ DB.logUser conn nick ident chan
  consoleWrite result
    
op :: String -> String -> String -> Net ()
op nick ident chan = do
  conn <- asks db
  o <- liftIO $ DB.isOp conn ident chan
  if o
    then write "MODE" $ chan ++ " +o " ++ nick
    else consoleWrite "not opping lol"

--
-- URLs
--
url :: String -> Net ()
url s = do
    link <- liftIO $ DB.getUrl s
    privmsg link

newurl :: String -> Net ()
newurl s = do
    c <- asks db
    strs <- liftIO $ splittan s
    result <- liftIO $ DB.addUrl c (head strs) (strs !! 1)
    privmsg result

--
-- Weather
--
weather' :: String -> Net ()
weather' city = do
    w <- liftIO $ getWeather city
    case w of
        Just Weather {} -> privmsg $ tellWeather $ fromJust w
        _ -> privmsg "juuh en tiärä..."

tellWeather :: Weather -> String
tellWeather w =
    "Elikkäs " ++ description w ++ ", lämpöä " ++ show (temp w) ++ " astetta ja tuuleepi " ++ show (wSpeed w) ++ " m/s suunnasta " ++ show (wDeg w)
