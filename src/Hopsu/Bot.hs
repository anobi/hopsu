module Hopsu.Bot where

import Network

import System.IO
import System.Exit
import System.Time
import System.Directory

import Database.HDBC.Sqlite3

import Text.Printf

import Data.List
import Data.Maybe
import Data.String as S

import Control.Arrow
import Control.Monad.Reader
import Control.Exception as EX

import Prelude

import Hopsu.Types
import Hopsu.Config
import Hopsu.Db as DB
import Hopsu.Handler as Handler
import Hopsu.Heather

--
-- engine part, under the hood etc
--

hopsu :: IO ()
hopsu = bracket connect dc botloop
    where
        dc          = hClose . socket
        botloop st  = EX.catch ( runReaderT Hopsu.Bot.run st) (\(SomeException e) -> print e)

-- open the connection and fire up the bot
connect :: IO Bot
connect = do
    d <- getHomeDirectory
    c <- readConfig $ d ++ "/.hopsu/hopsu.config"
    t <- getClockTime
    dbconn <- connectSqlite3 $ d ++ "/.hopsu/hopsu.db"
    h <- connectTo (server c) (PortNumber (fromIntegral (port c)))
    hSetBuffering h NoBuffering
    hSetEncoding h utf8
    return $ Bot t h c dbconn

-- connect to irc server, join a channel and start listening
run :: Net ()
run = do
    c <- asks config
    write "NICK" $ nick c
    write "USER" $ nick c ++ " 0 * :hopperbot"
    write "JOIN" $ chan c ++ " " ++ pass c
    asks socket >>= listen

-- listen and respond to stuff from irck
listen :: Handle -> Net ()
listen h = loop $ do
    s <- init `fmap` liftIO (hGetLine h)
    liftIO $ putStrLn s

    -- react to irc happenings (not user commands)
    -- join messages are like :nick!ident@server JOIN :#channel
    if ping s then pong s
    else if join s then userjoin (uNick s) (ident s) (ch s)
    else eval $ clean s
    where
        loop a      = a >> loop a
        clean       = drop 1 . dropWhile (/= ':') . drop 1
        ping x      = "PING :" `isPrefixOf` x
        pong x      = write "PONG" $ ':' : drop 6 x
        join x      = "JOIN" `isInfixOf` x
        hello x     = greet $ drop 1 x --drop the leading :
        uNick x     = getNick $ words x
        ch x        = getChan $ words x
        ident x     = getIdent $ words x

-- send stuff to irck
write :: String -> String -> Net()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h   "%s %s\r\n" s t
    liftIO $ printf      "> %s %s\n" s t

consoleWrite :: String -> Net()
consoleWrite s = liftIO $ printf "%s\n" s

-- say something to some(one/where)
privmsg :: String -> Net ()
privmsg s = asks config >>= \c -> write "PRIVMSG" $ chan c ++ " :" ++ s

-- handle and obey the master's orders
eval :: String -> Net ()
eval x | "!id "     `isPrefixOf` x = privmsg $ drop 4 x
eval x | "!url "    `isPrefixOf` x = url $ drop 5 x
eval x | "!sää "    `isPrefixOf` x = weather' $ drop 5 x
eval x | "!newurl " `isPrefixOf` x = newurl $ drop 8 x

eval "!uptime"  = uptime >>= privmsg
eval "!quit"    = write "QUIT" ":!ulos" >> liftIO exitSuccess
eval _          = return ()

-- tell the uptime
uptime :: Net String
uptime = do
    now <- liftIO getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

-- function to wrap all the joining actions in
userjoin :: String -> String -> String -> Net()
userjoin usernick ident ch = do
    user <- User {ident = ident, nick = usernick, chan = ch}
    db <- asks db
    write Handler.logUser db user
    write Handler.op db user

getNick :: [String] -> String
getNick s = takeWhile (/= '!') $ drop 1 $ head s

getIdent :: [String] -> String
getIdent s = drop 1 $ dropWhile (/= '!') $ head s

getChan :: [String] -> String
getChan s = drop 1 $ last s

-- say hello to the guy who just joined the channel
greet :: String -> Net ()
greet x =
    privmsg $ "Eyh " ++ guy ++ ", " ++ greeting
    where
        guy = takeWhile (/= '!') x
        greeting = "loool"

weather' :: String -> Net ()
weather' city = do
    w <- liftIO $ getWeather city
    case w of
        Just (Weather {}) -> privmsg $ tellWeather $ fromJust w
        _ -> privmsg "juuh en tiärä..."

--
-- housekeeping stuff
--

tellWeather :: Weather -> String
tellWeather w =
    "Elikkäs " ++ description w ++ ", lämpöä " ++ show (temp w) ++ " astetta ja tuuleepi " ++ show (wSpeed w) ++ " m/s suunnasta " ++ show (wDeg w)

pretty :: TimeDiff -> String
pretty td =
    unwords $ map (uncurry (++) . first show) $
    if null diffs then [(0,"s")] else diffs
    where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                      in (tot',(sec',typ):acc)
          metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
          diffs = filter((/= 0) . fst) $ reverse $ snd $ 
                  foldl' merge (tdSec td,[]) metrics

