{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as BSC
import           Data.Time.Clock
import           Text.ParserCombinators.ReadPrec
import           Text.Read

-- Wrapped TimeStamp type for printing/parsing instances

newtype TimeStamp = TimeStamp { unTimeStamp :: UTCTime }

tsFormat :: String
tsFormat = "%Y-%m-%dT%H:%M"

-- Location

data Location = Location Int Int

data LocationUOM = Kilometre
                 | Mile
                 | Metre

instance Show LocationUOM where
    show Kilometre = "km"
    show Mile      = "mi"
    show Metre     = "m"

instance Read LocationUOM where
    readPrec = do
        x <- look
        case x of
            "km" -> get >> get >> return Kilometre
            "mi" -> get >> get >> return Mile
            "m"  -> get >> return Metre
            _    -> pfail

-- Temperature

type Temperature = Int

data TemperatureUOM = Celsius
                    | Fahrenheit
                    | Kelvin

instance Show TemperatureUOM where
    show Celsius    = "C"
    show Fahrenheit = "F"
    show Kelvin     = "K"

instance Read TemperatureUOM where
    readPrec = do
        x <- look
        case x of
            "C" -> get >> return Celsius
            "F" -> get >> return Fahrenheit
            "K" -> get >> return Kelvin
            _   -> pfail
-- Observatory

newtype Observatory = Observatory ByteString
  deriving (Eq, Ord)

instance Show Observatory where
    show (Observatory x) = BSC.unpack x

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

-- Normalised LogLine in metre-kelvin
data StatsLine = StatsLine (Double, Double) Double Observatory
