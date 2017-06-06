module Hopsu.Config where

import Control.Monad.Error
import Control.Applicative
import Data.ConfigFile

import Hopsu.Types

-- read the config file
readConfig :: String -> IO Config
readConfig f = do
    rv <- runError T $ do
        c <- fmap join $ fmap (readfile emptyCP f)
        return $ Config <$> (conf "server")
                        <*> (conf "port")
                        <*> (conf "nick")
                        <*> (conf "chan")
                        <*> (conf "pass")
        where conf = (\s -> get c "Connection" s)
    either (\x -> error (snd x)) (\x -> return x) rv
