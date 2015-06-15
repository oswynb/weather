{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.ByteString (ByteString)
import           Data.Time.Clock

-- Wrapped TimeStamp type for printing/parsing instances

newtype TimeStamp = TimeStamp { unTimeStamp :: UTCTime }

tsFormat :: String
tsFormat = "%Y-%m-%dT%H:%M"

-- Location

data Location = Location Int Int

data LocationUOM = Kilometre
                 | Mile
                 | Metre

-- Temperature

type Temperature = Int

data TemperatureUOM = Celsius
                    | Fahrenheit
                    | Kelvin

-- Observatory

newtype Observatory = Observatory ByteString

-- UOM pairing
--
observatoryLocationUOM :: Observatory -> LocationUOM
observatoryLocationUOM (Observatory x)
    | x == "AU" = Kilometre
    | x == "US" = Mile
    | x == "FR" = Metre
    | otherwise = Kilometre

observatoryTemperatureUOM :: Observatory -> TemperatureUOM
observatoryTemperatureUOM (Observatory x)
    | x == "AU" = Celsius
    | x == "US" = Fahrenheit
    | x == "FR" = Kelvin
    | otherwise = Kelvin

-- Log lines

data LogLine = LogLine TimeStamp Location Temperature Observatory
