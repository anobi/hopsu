module Hopsu.Types where

import Network

type Net = ReaderT Bot IO

data Bot = Bot {
    starttime   :: ClockTime, 
    socket      :: Handle, 
    config      :: Config, 
    db          :: Connection
}

data Config = Config {
    server  :: String, 
    port    :: Int, 
    nick    :: String, 
    chan    :: String, 
    pass    :: String
} deriving (Read, Show)

data User = User {
    nick        :: String,
    identd      :: String,
    channels    :: [Channel]
} 

data Channel = Channel {
    channel :: String,
    network :: Network,
    modes   :: [String],    
    mods    :: [User]
}

data Network = Network {
    name    :: String,
    servers :: [String]
}
