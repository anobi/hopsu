module Hopsu.Bot where

import Network
import System.IO
import System.Exit
import System.Time
import System.Directory
import Text.Printf
import Data.List
import Data.ConfigFile
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Control.Monad.Error
import Prelude hiding (catch)

import Hopsu.Db

data Bot = Bot {starttime :: ClockTime, socket :: Handle, config :: Config, db :: Db}
data Config = Config {server :: String, port :: Int, nick :: String, chan :: String, pass :: String} deriving (Read, Show)
type Net = ReaderT Bot IO

--
-- engine part, under the hood etc
--


hopsu :: IO ()
hopsu = bracket connect disconnect loop
    where
        disconnect  = hClose . socket
        loop st     = catch (runReaderT run st) (\(SomeException _) -> return ())

-- open the connection and fire up the bot
connect :: IO Bot
connect = do
    d <- getHomeDirectory
    c <- readConfig (d ++ "/.hopsu/hopsurc.config") 
    t <- getClockTime
    db <- runDb (d ++ "/.hopsu/hopsu.db")
    h <- connectTo (server c) (PortNumber (fromIntegral (port c)))
    hSetBuffering h NoBuffering
    return (Bot t h c db)

-- connect to irc server, join a channel and start listening
run :: Net ()
run = do
    c <- asks config
    write "NICK" (nick c)
    write "USER" ((nick c) ++ " 0 * :hopperbot")
    write "JOIN" ((chan c) ++ " " ++ (pass c))
    asks socket >>= listen

-- listen and respond to stuff from irck
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)

    -- react to irc happenings (not user commands)
    -- join messages are like :nick!ident@server JOIN :#channel
    if ping s then pong s
    else if join s then hello s
    else eval (clean s)
    where
        forever a   = a >> forever a
        clean       = drop 1 . dropWhile (/= ':') . drop 1
        ping x      = "PING :" `isPrefixOf` x
        pong x      = write "PONG" (':' : drop 6 x)
        join x      = "JOIN" `isInfixOf` x
        hello x     = greet $ drop 1 x --drop the leading :

-- send stuff to irck
write :: String -> String -> Net()
write s t = do
    h <- asks socket
    io $ hPrintf h   "%s %s\r\n" s t
    io $ printf      "> %s %s\n" s t

-- say something to some(one/where)
privmsg :: String -> Net ()
privmsg s = do
    c <- asks config
    write "PRIVMSG" ((chan c) ++ " :" ++ s) 

-- misc stuff
io :: IO a -> Net a
io = liftIO

-- read the config file
readConfig :: String -> IO Config
readConfig f = do
    rv <- runErrorT $ do
        cp <- join $ liftIO $ readfile emptyCP f
        let x = cp

        -- todo redo, horrible
        s <- get x "Connection" "server"
        p <- get x "Connection" "port"
        n <- get x "Connection" "nick"
        c <- get x "Connection" "chan"
        ps <- get x "Connection" "pass"

        return (Config { server = s
                        ,port = p
                        ,nick = n
                        ,chan = c
                        ,pass = ps
                        })

    either (\x -> error (snd x)) (\x -> return x) rv

--
-- here be the functionality, the good stuff
--

-- handle and obey the master's orders
eval :: String -> Net ()
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval x | "!url" `isPrefixOf` x = url x
eval "!uptime"  = uptime >>= privmsg
eval "!quit"    = write "QUIT" ":!ulos" >> io (exitWith ExitSuccess)
eval _          = return ()

-- tell the uptime
uptime :: Net String
uptime = do
    now <- io getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

url :: String -> Net ()
url s = do
    c <- asks connection
    link <- geturl c s
    privmsg $ "" ++ link

-- say hello to the guy who just joined the channel
greet :: String -> Net ()
greet x =
    privmsg ("Hei " ++ nick ++ ", " ++ greet)
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

