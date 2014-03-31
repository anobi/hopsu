module Hopsu.Config where

import Data.ConfigFile
import Control.Monad.Error

data Config = Config {server :: String, port :: Int, nick :: String, chan :: String, pass :: String} deriving (Read, Show)


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
