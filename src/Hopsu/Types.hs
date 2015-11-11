module Hopsu.Types where

import System.Time
import System.IO
import Control.Monad.Reader
import Database.HDBC.Sqlite3

type Net = ReaderT Bot IO

data Bot = Bot {
    starttime :: ClockTime
    , socket  :: Handle
    , config  :: Config
    , db      :: Connection
    }

data Config = Config {
    server    :: String
    , port    :: Int
    , botNick :: String
    , botChan :: String
    , pass    :: String
    } deriving (Read, Show)

data User = User {
    ident  :: String
    , nick :: String
    , chan :: String
    }

data Url = Url {
    alias :: String
    , url :: String
    }

data IrcCommand = IrcCommand {
    command  :: String
    , params :: String
    }
