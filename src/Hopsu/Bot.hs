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
import Hopsu.Handler as Handler

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
    write (toCmd "NICK" (botNick c))
    write (toCmd "USER" (botNick c ++ " 0 * :hopperbot"))
    write (toCmd "JOIN" (botChan c ++ " " ++ pass c))
    asks socket >>= listen

-- listen and respond to stuff from irck
listen :: Handle -> Net ()
listen h = botLoop $ do
    s <- init `fmap` liftIO (hGetLine h)
    liftIO $ putStrLn s

    -- react to irc happenings (not user commands)
    -- join messages are like :nick!ident@server JOIN :#channel
    if ping s then pong s
    else if userJoin s then handleJoin (toUser (uNick s) (uIdent s) (ch s))
    else eval $ clean s
    where
        botLoop a   = a >> botLoop a
        clean       = drop 1 . dropWhile (/= ':') . drop 1
        ping x      = "PING :" `isPrefixOf` x
        pong x      = write $ toCmd "PONG" (':' : drop 6 x)
        userJoin x  = "JOIN" `isInfixOf` x
        uNick x     = getNick $ words x
        ch x        = getChan $ words x
        uIdent x    = getIdent $ words x

toCmd :: String -> String -> IrcMessage
toCmd c p = IrcMessage {command = c, params = p}

toUser :: String -> String -> String -> User
toUser i n c = User {ident = i, nick = n, chan = c}

-- send commands to irck
write :: IrcMessage -> Net()
write msg = do
    h <- asks socket
    _ <- liftIO $ hPrintf h   "%s %s\r\n" s t
    liftIO $ printf      "> %s %s\n" s t
    where s = command msg
          t = params msg

consoleWrite :: String -> Net()
consoleWrite s = liftIO $ printf "%s\n" s

-- say something to some(one/where)
privmsg :: String -> String -> IrcMessage
privmsg c s = toCmd "PRIVMSG" (c ++ " :" ++ s)

-- handle and obey the master's orders
-- todo: get channel
eval :: String -> Net ()
-- eval x | "!url "    `isPrefixOf` x = privmsg $ liftIO $ Handler.getUrl $ drop 5 x
-- eval x | "!newurl " `isPrefixOf` x = liftIO $ Handler.newurl $ drop 8 x
-- eval "!uptime"  = write $ privmsg  (botChan c) uptime
-- eval x | "!id "     `isPrefixOf` x = privmsg $ drop 4 x
-- eval x | "!sää "    `isPrefixOf` x = weather' $ drop 5 x
eval "!quit"    = write (toCmd "QUIT" ":!ulos") >> liftIO exitSuccess
eval _          = return ()

-- tell the uptime
uptime :: Net String
uptime = do
    now <- liftIO getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

-- function to wrap all the joining actions in
handleJoin :: User -> Net()
handleJoin user = do
    c <- asks db
    _ <- liftIO $ Handler.logUser c user
    isOp <- liftIO $ Handler.op c user
    case isOp of 
        Just (IrcMessage {}) -> write $ fromJust isOp
        _ -> return ()

getNick :: [String] -> String
getNick s = takeWhile (/= '!') $ drop 1 $ head s

getIdent :: [String] -> String
getIdent s = drop 1 $ dropWhile (/= '!') $ head s

getChan :: [String] -> String
getChan s = drop 1 $ last s

--
-- housekeeping stuff
--

pretty :: TimeDiff -> String
pretty td =
    unwords $ map (uncurry (++) . first show) $
    if null diffs then [(0,"s")] else diffs
    where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                      in (tot',(sec',typ):acc)
          metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
          diffs = filter((/= 0) . fst) $ reverse $ snd $ 
                  foldl' merge (tdSec td,[]) metrics

