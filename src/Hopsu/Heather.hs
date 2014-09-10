{-# LANGUAGE OverloadedStrings #-}

module Hopsu.Heather where

import Data.Aeson 
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit

data Weather = Weather { 
    weather         :: String,
    description     :: String,
    temp            :: Double,
    pressure        :: Double,
    humidity        :: Double,
    minTemp         :: Double,
    maxTemp         :: Double,
    wSpeed          :: Double,
    wDeg            :: Double
    } deriving (Show)

instance FromJSON Weather where
    parseJSON (Object v) = Weather <$> 
        ((head <$> v .: "weather")  >>= (.: "main"))        <*>
        ((head <$> v .: "weather")  >>= (.: "description")) <*>
        ((v .: "main")              >>= (.: "temp"))        <*>
        ((v .: "main")              >>= (.: "pressure"))    <*>
        ((v .: "main")              >>= (.: "humidity"))    <*>
        ((v .: "main")              >>= (.: "temp_min"))    <*>
        ((v .: "main")              >>= (.: "temp_max"))    <*>
        ((v .: "wind")              >>= (.: "speed"))       <*>
        ((v .: "wind")              >>= (.: "deg"))
    parseJSON _ = mzero

getWeather :: String -> IO (Maybe Weather)
getWeather s = do
    d <- simpleHttp $ "http://api.openweathermap.org/data/2.5/weather?units=metric&lang=fi&q=" ++ s
    return $ decode d 