module Hopsu.Bot where

import Network
import System.IO
import System.Exit
import System.Time
import System.Directory
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.Printf
import Data.List
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Control.Monad.Error
import Prelude hiding (catch)

import Hopsu.Db
import Hopsu.Config
--import Hopsu.Handler

data Bot = Bot {starttime :: ClockTime, socket :: Handle, config :: Config, db :: Connection}
type Net = ReaderT Bot IO

--
-- engine part, under the hood etc
--

hopsu :: IO ()
hopsu = bracket connect disconnect loop
    where
        disconnect  = hClose . socket
        loop st     = catch ( runReaderT Hopsu.Bot.run st) (\(SomeException _) -> return ())

-- open the connection and fire up the bot
connect :: IO Bot
connect = do
    d <- getHomeDirectory
    c <- readConfig $ d ++ "/.hopsu/hopsu.config"
    t <- getClockTime
    db <- connectSqlite3 $ d ++ "/.hopsu/hopsu.db"
    h <- connectTo (server c) (PortNumber (fromIntegral (port c)))
    hSetBuffering h NoBuffering
    return $ Bot t h c db

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
listen h = forever $ do
    s <- init `fmap` liftIO (hGetLine h)
    liftIO $ putStrLn s

    -- react to irc happenings (not user commands)
    -- join messages are like :nick!ident@server JOIN :#channel
    if ping s then pong s
    else if join s then hello s
    else eval $ clean s
    where
        forever a   = a >> forever a
        clean       = drop 1 . dropWhile (/= ':') . drop 1
        ping x      = "PING :" `isPrefixOf` x
        pong x      = write "PONG" $ ':' : drop 6 x
        join x      = "JOIN" `isInfixOf` x
        hello x     = greet $ drop 1 x --drop the leading :

-- send stuff to irck
write :: String -> String -> Net()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h   "%s %s\r\n" s t
    liftIO $ printf      "> %s %s\n" s t

-- say something to some(one/where)
privmsg :: String -> Net ()
privmsg s = do
    c <- asks config
    write "PRIVMSG" $ chan c ++ " :" ++ s

-- handle and obey the master's orders
eval :: String -> Net ()
eval x | "!id " `isPrefixOf` x = privmsg $ drop 4 x
eval x | "!url" `isPrefixOf` x = url x
eval "!uptime"  = uptime >>= privmsg
eval "!quit"    = write "QUIT" ":!ulos" >> liftIO (exitWith ExitSuccess)
eval _          = return ()

-- tell the uptime
uptime :: Net String
uptime = do
    now <- liftIO getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

url :: String -> Net ()
url s = do
    c <- asks db
    link <- liftIO $ geturl c s
    privmsg $ show link

-- say hello to the guy who just joined the channel
greet :: String -> Net ()
greet x =
    privmsg $ "Eyh " ++ nick ++ ", " ++ greet
    where
        nick = takeWhile (/= '!') x
        greet = "loool"

--
-- housekeeping stuff
--

pretty :: TimeDiff -> String
pretty td =
    unwords $ map (uncurry (++) . first show) $
    if null diffs then [(0,"s")] else diffs
    where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                      in (tot',(sec',typ):acc)
          metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
          diffs = filter((/= 0) . fst) $ reverse $ snd $ 
                  foldl' merge (tdSec td,[]) metrics

